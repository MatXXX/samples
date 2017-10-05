import ParGram
import AbsGram
import ErrM

import System.Environment
import System.Exit
import System.IO

import qualified Text.Read
import qualified Data.Map.Strict as HM
import qualified Data.Sequence as SQ
import qualified Data.Foldable as FLD
import qualified Data.List as L

import Control.Monad.State
import Control.Monad.Except

import InterpreterState
import Identifiers
import TypeCheck

noneError :: InterpreterState
noneError = throwError "Unassigned value used!"

builtinAssert :: [Expr] -> InterpreterState
builtinAssert [e] = do
    Just val <- evalExpr e
    case val of
        ExprBool b ->
            if not b then
                error $ "Assertion failed: " ++ show e
            else
                return Nothing
        ExprNone -> noneError

builtinAssert [e, msg] = do
    Just val <- evalExpr e
    Just msgVal <- evalExpr msg
    case val of
        (ExprBool b) ->
            if not b then do
                (env, _) <- get
                error $ "Assertion failed: " ++ showExprValue msgVal env ++ "\n               " ++ show e
            else
                return Nothing
        ExprNone -> noneError

builtinPrint :: [Expr] -> InterpreterState
builtinPrint es =
    builtinPrintHelper es ""
    where
        builtinPrintHelper [] acc = do
                liftIO $ putStrLn acc
                return Nothing
        builtinPrintHelper (e:es) acc = do
            Just val <- evalExpr e
            (env, _) <- get
            builtinPrintHelper es $ acc ++ showExprValue val env ++ " "

builtinReadLine :: InterpreterState
builtinReadLine = do
   line <- liftIO getLine
   return $ Just $ ExprString line

builtinStringToInt :: [Expr] -> InterpreterState
builtinStringToInt [e] = do
    Just (ExprString str) <- evalExpr e
    let i = Text.Read.readMaybe str :: Maybe Int in
        case i of
            Nothing -> return $ Just ExprNone
            (Just i2) -> return $ Just $ ExprInt i2

builtinIsNone :: [Expr] -> InterpreterState
builtinIsNone [e] = do
    Just val <- evalExpr e
    return $ Just $ ExprBool (isExprValueNone val)

builtinToString :: [Expr] -> InterpreterState
builtinToString [e] = do
    Just val <- evalExpr e
    return $ Just $ ExprString (show val)

isExprValueNone :: ExprValue -> Bool
isExprValueNone ExprNone = True
isExprValueNone _ = False

showExprValue :: ExprValue -> Env -> String
showExprValue ExprNone _ = "None"
showExprValue (ExprInt i) _ = show i
showExprValue (ExprBool b) _ = show b
showExprValue (ExprString s) _ = s
showExprValue (ExprStruct id values) env =
    let structDesc = structs env HM.! id
        fieldLocNames = map (\(name, (loc, _)) -> (loc, name)) $ HM.toList structDesc
        fieldNames = map snd $ L.sortOn fst $ fieldLocNames
        fieldNamesValues = zip fieldNames (FLD.toList values)
        nameValueToStr = \(name, value) -> name ++ ": " ++ (showExprValue value env)
        fieldsStr = L.intercalate ", " $ map nameValueToStr fieldNamesValues in 
            "Struct " ++ id ++ " { " ++ fieldsStr ++ " }"

typeOfExprValue :: ExprValue -> Type
typeOfExprValue (ExprInt _) = Int
typeOfExprValue (ExprBool _) = Bool
typeOfExprValue (ExprString _) = Str
typeOfExprValue (ExprFunc t args _ _) =
    let typeArgs = map (\(Arg _ targ) -> targ) args in
        case t of
            Nothing -> FVType typeArgs
            Just rt -> FType rt typeArgs

typeOfExprValue (ExprStruct name _) = StructType $ IdentDecl $ Ident name
typeOfExprValue ExprNone = None

parse s = let ts = myLexer s in case pProgram ts of
    Bad s -> Progra []
    Ok tree -> tree

getAddOp Plus = (+)
getAddOp Minus = (-)
getMulOp Times = (*)
getMulOp Div = div
getMulOp Mod = mod

getRelOp LTH = (<)
getRelOp LE = (<=)
getRelOp GTH = (>)
getRelOp GE = (>=)
getRelOp EQU = (==)
getRelOp NE = (/=)


evalExpr :: Expr -> InterpreterState
evalExpr (ELitInt n) = return $ Just $ ExprInt $ fromIntegral n

evalExpr (EAdd l Plus r) = do
    Just lr <- evalExpr l;
    Just rr <- evalExpr r;
    case (lr, rr) of
        (ExprInt e1, ExprInt e2) -> return $ Just $ ExprInt $ e1 + e2
        (ExprString e1, ExprString e2) -> return $ Just $ ExprString $ e1 ++ e2
        (ExprNone, _) -> noneError
        (_, ExprNone) -> noneError

evalExpr (EAdd l Minus r) = do
    Just lr <- evalExpr l;
    Just rr <- evalExpr r;
    case (lr, rr) of
        (ExprInt e1, ExprInt e2) -> return $ Just $ ExprInt $ e1 - e2
        (ExprNone, _) -> noneError
        (_, ExprNone) -> noneError

evalExpr (EMul l Times r) = do
    Just lr <- evalExpr l;
    Just rr <- evalExpr r;
    case (lr, rr) of
        (ExprInt e1, ExprInt e2) -> return $ Just $ ExprInt $ e1 * e2
        (ExprNone, _) -> noneError
        (_, ExprNone) -> noneError

evalExpr (EMul l op r) =
    let mulOp = getMulOp op in do
        Just lr <- evalExpr l;
        Just rr <- evalExpr r;
        case (lr, rr) of
            (ExprInt e1, ExprInt e2) -> 
                if e2 == 0 then
                    throwError "0 division"
                else
                    return $ Just $ ExprInt $ e1 `mulOp` e2
            (ExprNone, _) -> noneError
            (_, ExprNone) -> noneError

evalExpr (ERel l op r) = do
        Just le <- evalExpr l;
        Just re <- evalExpr r;
        case (le, re) of
            (ExprInt lr, ExprInt rr) -> 
                let relOp = getRelOp op in
                return $ Just $ ExprBool $ lr `relOp` rr
            (ExprBool lr, ExprBool rr) -> 
                let relOp = getRelOp op in
                return $ Just $ ExprBool $ lr `relOp` rr
            (ExprString lr, ExprString rr) -> 
                let relOp = getRelOp op in
                return $ Just $ ExprBool $ lr `relOp` rr
            (ExprNone, _) -> noneError
            (_, ExprNone) -> noneError

evalExpr ELitTrue =  return $ Just $ ExprBool True
evalExpr ELitFalse = return $ Just $ ExprBool False
evalExpr (EString s) = return $ Just $ ExprString s

evalExpr (Not e) = do
    Just (ExprBool v) <- evalExpr e
    return $ Just $ ExprBool (not v)

evalExpr (Neg e) = do
    Just (ExprInt v) <- evalExpr e
    return $ Just $ ExprInt (-v)

evalExpr (EOr l r) = do
        Just le <- evalExpr l
        Just re <- evalExpr r
        case (le, re) of
            (ExprBool lr, ExprBool rr) -> return $ Just $ ExprBool $ lr || rr
            (ExprNone, _) -> noneError
            (_, ExprNone) -> noneError

evalExpr (EAnd l r) = do
        Just le <- evalExpr l
        Just re <- evalExpr r
        case (le, re) of
            (ExprBool lr, ExprBool rr) -> return $ Just $ ExprBool $ lr && rr
            (ExprNone, _) -> noneError
            (_, ExprNone) -> noneError

evalExpr (EIdent (IdentAllD id)) =
    let ident = extractIdentDecl id in do
        (env, st) <- get
        let hm = vars env
            Var x _ = hm HM.! ident
            y = SQ.index st x in
                return $ Just y

evalExpr (EIdent (IdentAllBuiltin (IdentBuiltin (Builtin id)))) =
    case id of
        "@print" -> return $ Just $ ExprBuiltinFunc "print"
        "@assert" -> return $ Just $ ExprBuiltinFunc "assert"
        "@readLine" -> return $ Just $ ExprBuiltinFunc "readLine"
        "@stringToInt" -> return $ Just $ ExprBuiltinFunc "stringToInt"
        "@isNone" -> return $ Just $ ExprBuiltinFunc "isNone"
        "@toString" -> return $ Just $ ExprBuiltinFunc "toString"

evalExpr (EIdent (IdentAllField (IdentField id fields))) =
    let structId = extractIdentDecl id in do
        (env, st) <- get
        let hm = vars env
            (Var loc _) = hm HM.! structId
            struct = SQ.index st loc in
                case struct of
                    ExprNone -> noneError
                    _ -> return $ Just $ getStructField (structs env) struct fields


evalExpr (EFunc ret args stmts) = evalFunction (ExprFunc (Just ret)) args stmts

evalExpr (EVFunc args stmts) = evalFunction (ExprFunc Nothing) args stmts

evalExpr (EApp e actual) = do
    Just fun <- evalExpr e
    case fun of
        (ExprFunc ret formal stmts clos) -> do
            copy@(env1, st1) <- get
            put (clos, st1)
            evalFuncArgs actual formal copy
            ret <- evalStmts stmts
            (_, st2) <- get
            put (env1, st2)
            return ret
        (ExprBuiltinFunc name) ->
            case name of
                "print" -> builtinPrint actual
                "assert" -> builtinAssert actual
                "readLine" -> builtinReadLine
                "stringToInt" -> builtinStringToInt actual
                "isNone" -> builtinIsNone actual
                "toString" -> builtinToString actual
        ExprNone -> noneError

evalExpr (EStruct idDecl exprs) = do
    (Env _ structs, _) <- get
    let structName = extractIdentDecl idDecl
        desc = structs HM.! structName in
            evalExprStruct structName (L.sortOn fst $ map snd $ HM.toList desc) exprs SQ.empty

evalExpr (EStructNamed idDecl fieldExprs) = do
    (Env _ structs, _) <- get
    let structName = extractIdentDecl idDecl
        desc = structs HM.! structName in
            evalExprStructNamed structName desc fieldExprs (SQ.replicate (HM.size desc) ExprNone)

                -- ExprFunc with applied one argument  --
evalFunction :: ([Arg] -> [Stmt] -> Closure -> ExprValue) -> [Arg] -> [Stmt] -> InterpreterState
evalFunction ret args stmts = do
    (e, _) <- get
    return $ Just $ ret args stmts e

evalExprStructNamed :: String -> StructDescription -> [EStructNamedF] -> StructValue -> InterpreterState
evalExprStructNamed name _ [] acc = return $ Just $ ExprStruct name acc
evalExprStructNamed name sd (EStructNamedF id expr : fs) acc =
    let fieldName = extractIdentDecl id
        (loc, t) = sd HM.! fieldName in do
            Just ev <- evalExpr expr
            evalExprStructNamed name sd fs (SQ.update loc ev acc)


evalExprStruct :: String -> [StructFieldDesc] -> [Expr] -> StructValue -> InterpreterState
evalExprStruct name [] [] acc = return $ Just $ ExprStruct name acc
evalExprStruct _ [] _ acc = return Nothing
evalExprStruct _ _ [] acc = return Nothing
evalExprStruct name (fd:fds) (e:es) acc = do
    Just ev <- evalExpr e
    evalExprStruct name fds es (acc SQ.|> ev)

evalFuncArgs :: [Expr] -> [Arg] -> (Env, Storage) -> InterpreterState
evalFuncArgs actual formal = evalFuncArgsHelper (zip actual formal)
    where
        evalFuncArgsHelper :: [(Expr, Arg)] -> (Env, Storage) -> InterpreterState
        evalFuncArgsHelper [] _ = return Nothing
        evalFuncArgsHelper ((a, Arg id t):ps) state = do
            let str = extractIdentDecl id in
                case t of
                    FTypeArg ty -> do
                        av <- evalExprInState a state
                        addVariable str ty av
                    FTypeArgRef _ -> setReference str a $ fst state
            evalFuncArgsHelper ps state
        evalExprInState e state = do
            copy <- get;
            put state;
            Just val <- evalExpr e;
            put copy;
            return val

parseFunArgs :: [Arg] -> InterpreterState
parseFunArgs [] = return Nothing
parseFunArgs (Arg id (FTypeArg t):xs) = do
    addVariable (extractIdentDecl id) t ExprNone
    parseFunArgs xs
parseFunArgs (Arg id (FTypeArgRef t):xs) = do
    addVariableRef (extractIdentDecl id) t
    parseFunArgs xs

evalStmtExp :: Expr -> InterpreterState
evalStmtExp = evalExpr

evalConditional :: Expr -> [Stmt] -> [CondElif] -> [Stmt] -> InterpreterState
evalConditional cond if' [] else' = do
    Just val <- evalExpr cond
    case val of
        ExprBool b ->
            if b then
                evalBlockHandleEnv if'
            else
                evalBlockHandleEnv else'
        ExprNone -> noneError

evalConditional cond if' elif' else' = do
    Just val <- evalExpr cond
    case val of
        ExprBool b ->
            if b
            then evalBlockHandleEnv if'
            else let (CondElif expr stmts) = head elif' in
                evalConditional expr stmts (tail elif') else'
        ExprNone -> noneError

evalStmtCond :: StmtCond -> InterpreterState
evalStmtCond (Cond cond if' elif') = evalConditional cond if' elif' [StmtEmpty]
evalStmtCond (CondElse cond if' elif' else') = evalConditional cond if' elif' else'
evalStmtCond w@(While cond if') = evalConditional cond (if' ++ [StmtCond w]) [] [StmtEmpty]


evalStmt :: Stmt -> InterpreterState
evalStmt (StmtNoBlock a) = evalStmtNB a
evalStmt (StmtBlock (Block a)) = evalBlockHandleEnv a
evalStmt StmtEmpty = return Nothing
evalStmt (StmtCond x) = evalStmtCond x

typeOfFunction :: Maybe Type -> [Arg] -> Type
typeOfFunction Nothing args = FVType $ map (\(Arg _ t) -> t) args
typeOfFunction (Just rt) args = FType rt $ map (\(Arg _ t) -> t) args

evalRecursiveFunction :: String -> Maybe Type -> [Arg] -> [Stmt] -> InterpreterState
evalRecursiveFunction id ret args stmts =
    let t = typeOfFunction ret args in do
        addVariable id t ExprNone
        Just f <- evalFunction (ExprFunc ret) args stmts
        setVariable id f
        return Nothing

evalStmtDeclInit :: String -> Expr -> InterpreterState
evalStmtDeclInit id (EFunc ret args stmts) = evalRecursiveFunction id (Just ret) args stmts
evalStmtDeclInit id (EVFunc args stmts) = evalRecursiveFunction id Nothing args stmts
evalStmtDeclInit id e = do
    Just val <- evalExpr e;
    addVariable id (typeOfExprValue val) val

evalStmtDecl :: String -> Type -> InterpreterState
evalStmtDecl id t = addVariable id t ExprNone

evalStmtDeclInitType :: String -> Type -> Expr -> InterpreterState
evalStmtDeclInitType id _ = evalStmtDeclInit id

evalStmtNBAssField :: IdentField -> Expr -> InterpreterState
evalStmtNBAssField (IdentField structid fields) e = do
    Just val <- evalExpr e;
    (env, st) <- get
    let hm = vars env
        strs = structs env
        id = extractIdentDecl structid
        Var loc t = hm HM.! id in do
            let st2 = SQ.update loc (setFieldValue strs t (SQ.index st loc) fields val) st in
                put (env, st2);
            return Nothing

evalStmtNBAssDecl :: String -> Expr -> InterpreterState
evalStmtNBAssDecl id e = do
    Just val <- evalExpr e;
    setVariable id val;

evalStmtNBStructDecl :: String -> [StructFieldDecl] -> InterpreterState
evalStmtNBStructDecl name fields = evalStmtNBStructDeclHelper name fields 0 HM.empty
    where
        evalStmtNBStructDeclHelper :: String -> [StructFieldDecl] -> Int -> StructDescription -> InterpreterState
        evalStmtNBStructDeclHelper name [] _ acc = addStruct name acc
        evalStmtNBStructDeclHelper name (StructFieldDecl id t:fs) index acc =
            evalStmtNBStructDeclHelper name fs (index + 1) (HM.insert (extractIdentDecl id) (index, t) acc)

evalStmtNB :: StmtNB -> InterpreterState
evalStmtNB (SExp e) = evalStmtExp e
evalStmtNB (DeclInit idDecl expr) = evalStmtDeclInit (extractIdentDecl idDecl) expr
evalStmtNB (Decl idDecl t) = evalStmtDecl (extractIdentDecl idDecl) t
evalStmtNB (DeclInitType idDecl t expr) = evalStmtDeclInitType (extractIdentDecl idDecl) t expr
evalStmtNB (AssDecl idDecl expr) = evalStmtNBAssDecl (extractIdentDecl idDecl) expr
evalStmtNB (StructDecl idDecl fields) = evalStmtNBStructDecl (extractIdentDecl idDecl) fields
evalStmtNB (AssField fields expr) = evalStmtNBAssField fields expr

evalStmts :: [Stmt] -> InterpreterState
evalStmts [] = return Nothing
evalStmts (StmtNoBlock (Ret e):_) = evalExpr e
evalStmts (StmtNoBlock VRet:_) = return $ Just ExprNone
evalStmts (x:xs) = do
    v <- evalStmt x
    case v of
        Nothing -> evalStmts xs
        Just val -> return $ Just val

evalBlockHandleEnv :: [Stmt] -> InterpreterState
evalBlockHandleEnv stmts = do
    (env1, st1) <- get
    ret <- evalStmts stmts
    (_, st2) <- get
    put (env1, SQ.take (SQ.length st1) st2)
    return ret

evalProgram :: Program -> InterpreterState
evalProgram (Progra []) = return Nothing
evalProgram (Progra xs) = evalStmts xs

runInterpreter :: String -> IO()
runInterpreter code =
    case parse code of
        Progra [] -> putStrLn "Parse error"
        p -> 
            case typeCheck p of
                Left err -> putStrLn err
                Right _ -> do
                    results <- runExceptT $ runStateT (evalProgram p) emptyState;
                    case results of
                        Left s -> do
                            putStrLn $ "Error occured: " ++ s
                        Right _ -> do
                            return ()
    where emptyState = (Env { vars = HM.empty, structs = HM.empty }, SQ.empty)


main = do
    args <- getArgs
    if L.length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage " ++ progName ++ " <script file>"
        exitFailure
    else do
        code <- readFile $ args !! 0
        runInterpreter code

-- TODO: Shadowing variables in blocks
