module TypeCheck (
    typeCheck
) where

import AbsGram

import Identifiers
import InterpreterState

import Debug.Trace

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as HM
import qualified Data.List as L

type TypeCheckState = StateT (Env, Type, Bool) (Either String) ()

typeCheck :: Program -> Either String Bool
typeCheck (Progra s) = typeCheckStmtsRun s emptyEnv None
    where
        emptyEnv = Env { vars = HM.empty, structs = HM.empty }

typeCheckStmtsRun :: [Stmt] -> Env -> Type -> Either String Bool
typeCheckStmtsRun stmts env ret = do
    case runStateT (typeCheckStmts stmts) (env, ret, False) of
        Left s -> Left s
        Right (_, state) ->
            let (_, _, hasReturned) = state in
                Right hasReturned

typeCheckStmts :: [Stmt] -> TypeCheckState
typeCheckStmts [] = lift $ Right ()
typeCheckStmts (s:ss) = do
    typeCheckStmt s
    typeCheckStmts ss

typeCheckStmt :: Stmt -> TypeCheckState
typeCheckStmt StmtEmpty = lift $ Right ()
typeCheckStmt (StmtNoBlock stmtNB) = typeCheckNB stmtNB
typeCheckStmt (StmtBlock (Block stmts)) = typeCheckStmts stmts -- add some block specyfic stuff
typeCheckStmt (StmtCond stmtCond) = typeCheckCond stmtCond

typeCheckNB :: StmtNB -> TypeCheckState
typeCheckNB (AssDecl ident expr) = typeCheckAssDecl (extractIdentDecl ident) expr
typeCheckNB (AssField ident expr) = typeCheckAssField ident expr
-- typeCheckNB (Alias identDecl t) = return ()
typeCheckNB (Ret expr) = do
    (e, t, _) <- get
    typeCheckTypeExpr t expr
    put (e, t, True)


typeCheckNB VRet = do
    (e, t, _) <- get
    if t == None then do
        put (e, t, True)
        lift $ Right ()
    else
        lift $ Left "Void return error"

typeCheckNB (SExp expr) = typeCheckExpr expr
typeCheckNB (Decl identDecl t) = do
    (e, rt, r) <- get
    --Add redeclaration guard
    let newVars = HM.insert (extractIdentDecl identDecl) Var {loc = 0, varType = t} (vars e) in do
        put (e { vars = newVars }, rt, r)
        lift $ Right ()

typeCheckNB (DeclInit identDecl expr@(EFunc ret args stmts)) = do
    (e, rt, r) <- get
    --Add redeclaration guard
    let ft = FType ret $ map (\(Arg _ t) -> t) args
        newVars = HM.insert (extractIdentDecl identDecl) Var {loc = 0, varType = ft} (vars e)
        t = getExprType expr e { vars = newVars } in
        case t of
            Left s -> lift $ Left $ "Incorrect RHS, " ++ (show expr) ++ "\n>>" ++ s
            Right _ -> do
                put (e { vars = newVars }, rt, r)
                lift $ Right ()

typeCheckNB (DeclInit identDecl expr@(EVFunc args stmts)) = do
    (e, rt, r) <- get
    --Add redeclaration guard
    let tt = FVType $ map (\(Arg _ t) -> t) args
        newVars = HM.insert (extractIdentDecl identDecl) Var {loc = 0, varType = tt} (vars e)
        t = getExprType expr e { vars = newVars } in
        case t of
            Left s -> lift $ Left $ "Incorrect RHS, " ++ (show expr) ++ "\n>>" ++ s
            Right tt -> do
                put (e { vars = newVars }, rt, r)
                lift $ Right () -- already added

typeCheckNB (DeclInit identDecl expr) = do
    (e, rt, r) <- get
    --Add redeclaration guard
    let t = getExprType expr e in
        case t of 
            Left s -> lift $ Left $ "Incorrect RHS, " ++ (show expr) ++ "\n>>" ++ s
            Right tt ->
                let newVars = HM.insert (extractIdentDecl identDecl) Var {loc = 0, varType = tt} (vars e) in do
                    put (e { vars = newVars }, rt, r)
                    lift $ Right ()

typeCheckNB (DeclInitType identDecl t expr) = do
    typeCheckTypeExpr t expr
    (e, rt, r) <- get
    let newVars = HM.insert (extractIdentDecl identDecl) Var {loc = 0, varType = t} (vars e) in do
        put (e { vars = newVars }, rt, r)
        lift $ Right ()

typeCheckNB (StructDecl identDecl structFieldDecls) = do
    --check if identDecl is not yet declared
    (e, t, _) <- get
    let strcts = structs e
        name = extractIdentDecl identDecl in
        if strcts HM.!? name /= Nothing then do
            lift $ Left $ "Redeclaretion of " ++ name
        else
            addStructDescription name structFieldDecls
    where
        addStructDescription :: String -> [StructFieldDecl] -> TypeCheckState
        addStructDescription name fields = addStructDescriptionHelper name fields 0 HM.empty
            where
                addStructDescriptionHelper :: String -> [StructFieldDecl] -> Int -> StructDescription -> TypeCheckState
                addStructDescriptionHelper name [] _ acc = do
                        (env, t, r) <- get
                        put (env { structs = HM.insert name acc (structs env) }, t, r)
                        lift $ Right ()

                addStructDescriptionHelper name (StructFieldDecl id t:fs) index acc =
                    case t of
                        StructType sid -> do
                            (Env _ structs, _, _) <- get
                            case structs HM.!? extractIdentDecl sid of
                                Nothing -> lift $ Left $ "Undeclared identifier " ++ (extractIdentDecl sid)
                                Just _ -> addStructDescriptionHelper name fs (index + 1) (HM.insert (extractIdentDecl id) (index, t) acc)
                        _ -> addStructDescriptionHelper name fs (index + 1) (HM.insert (extractIdentDecl id) (index, t) acc)

typeCheckCond :: StmtCond -> TypeCheckState
typeCheckCond (Cond expr stmts condElifs) = do
    typeCheckTypeExpr Bool expr
    typeCheckStmts stmts -- add some block specyfic stuff
    typeCheckCondElif condElifs
typeCheckCond (CondElse expr stmtsIf condElifs stmtsElse) = do
    typeCheckTypeExpr Bool expr
    typeCheckStmts stmtsIf --asdf = add some block specyfic stuff
    typeCheckCondElif condElifs
    typeCheckStmts stmtsElse -- asdf
typeCheckCond (While expr stmts) = do
    typeCheckTypeExpr Bool expr
    typeCheckStmts stmts -- asdf

typeCheckCondElif :: [CondElif] -> TypeCheckState
typeCheckCondElif [] = lift $ Right ()
typeCheckCondElif (CondElif expr stmts:cs) = do
    typeCheckTypeExpr Bool expr
    typeCheckStmts stmts -- asdf
    typeCheckCondElif cs

typeCheckAssDecl :: String -> Expr -> TypeCheckState
typeCheckAssDecl id expr = do
    (e, _, _) <- get
    let hm = vars e in
        case hm HM.!? id of
            Nothing -> lift $ Left $ "Undeclared identifier: " ++ id
            Just (Var _ t) -> typeCheckTypeExpr t expr

typeCheckAssField :: IdentField -> Expr -> TypeCheckState
typeCheckAssField ident expr = do
    (e, _, _) <- get
    let IdentField id fields = ident
        name = extractIdentDecl id in
        case vars e HM.!? name of
            Nothing -> lift $ Left $ "Undeclared identifier: " ++ name
            Just (Var _ t) ->
                case getFieldType e t fields of
                    Nothing -> lift $ Left $ "Unknown identifier: " ++ name ++ (unwords $ map extractIdentStructField fields)
                    Just t -> typeCheckTypeExpr t expr
    where
        getFieldType :: Env -> Type -> [StructField] -> Maybe Type
        getFieldType env structType [] = Just structType
        getFieldType env (StructType ident) (f:fs) = do
            ft <- extractFieldType
            getFieldType env ft fs
                where
                    extractFieldType =
                        let strcts = structs env in do
                            strDesc <- strcts HM.!? (extractIdentDecl ident)
                            (_, fieldType) <- strDesc HM.!? (extractIdentStructField f)
                            return fieldType
        getFieldType _ _ _ = Nothing

typeCheckTypeExpr :: Type -> Expr -> TypeCheckState
typeCheckTypeExpr t e = do
    (env, _, _) <- get
    case getExprType e env of
        Right et -> 
            if t == et then
                lift $ Right ()
            else
                lift $ Left $ "Cannot assign type " ++ (show et) ++ " to " ++ (show t) ++ " in " ++ (show e)
        Left s ->
            lift $ Left $ "Cannot evalueate expression " ++ (show e) ++ "\n>>" ++ s

typeCheckExpr :: Expr -> TypeCheckState
typeCheckExpr e = do
    (env, _, _) <- get
    case getExprType e env of
        Right _ -> lift $ Right ()
        Left s -> lift $ Left $ "Incorrect expression " ++ (show e) ++ "\n>>" ++ s

getExprType :: Expr -> Env -> Either String Type

getExprType (EIdent (IdentAllBuiltin (IdentBuiltin (Builtin id)))) _ =
    case id of
        "@print" -> Right $ BuiltinFuncType "print"
        "@assert" -> Right $ BuiltinFuncType "assert"
        "@readLine" -> Right $ BuiltinFuncType "readLine"
        "@stringToInt" -> Right $ BuiltinFuncType "stringToInt"
        "@isNone" -> Right $ BuiltinFuncType "isNone"
        "@toString" -> Right $ BuiltinFuncType "toString"
        _ -> Left $ "Unknown builtin identifier " ++ id

getExprType (EIdent (IdentAllField (IdentField id fields))) env =
    let structId = extractIdentDecl id
        hm = vars env in
        case hm HM.!? structId of
            Nothing -> Left $ "Undeclared identifier " ++ structId
            Just (Var _ t) ->
                case getStructFieldType t fields of
                    Right t -> Right t
                    Left s -> Left $ "Struct accessor error, " ++ s
    where
        getStructFieldType :: Type -> [StructField] -> Either String Type
        getStructFieldType structType [] = Right structType
        getStructFieldType (StructType id) (f:fs) =
            let strcts = structs env
                structName = extractIdentDecl id
                structDesc = strcts HM.!? structName in
                    case structDesc of
                        Nothing -> Left $ structName ++ " is not decalred"
                        Just sd ->
                            let fieldName = extractIdentStructField f
                                fieldDesc = sd HM.!? fieldName in
                                    case fieldDesc of
                                        Nothing -> Left $ fieldName ++ " is not a field of " ++ structName
                                        Just (_, t) -> getStructFieldType t fs
        getStructFieldType _ _ = Left "Not a struct"

getExprType (EIdent identAll) env =
    case (vars env) HM.!? (extractIdentAll identAll) of
        Nothing -> Left $ "No such identifier " ++ (extractIdentAll identAll)
        Just (Var _ t) -> Right t

getExprType (ELitInt _) env = Right Int
getExprType ELitTrue env = Right Bool
getExprType ELitFalse env = Right Bool
getExprType (EString _) env = Right Str
getExprType (Not e) env = do
    t <- getExprType e env
    if t == Bool then
        Right Bool
    else
        Left $ "Cannot negate non boolean, " ++ (show e)
getExprType (Neg e) env = do
    t <- getExprType e env
    if t == Int then
        Right Int
    else
        Left $ "Error, NOT, " ++ (show e)
getExprType (EMul e1 _ e2) env = do
    t1 <- getExprType e1 env
    t2 <- getExprType e2 env
    if t1 == Int && t2 == Int then
        Right Int
    else
        Left $ "Error, MUL, " ++ (show e1) ++ (show e2)
getExprType (EAdd e1 Plus e2) env = do
    t1 <- getExprType e1 env
    t2 <- getExprType e2 env
    if t1 == t2 && (t1 == Int || t1 == Str) then
        return t1
    else
        Left $ "Error, PLUS, " ++ (show e1) ++ (show e2)

getExprType (EAdd e1 Minus e2) env = do
    t1 <- getExprType e1 env
    t2 <- getExprType e2 env
    if t1 == t2 && t1 == Int then
        Right Int
    else
        Left $ "Error, MINUS, " ++ (show e1) ++ (show e2)

getExprType (ERel e1 _ e2) env = do
    t1 <- getExprType e1 env
    t2 <- getExprType e2 env
    if t1 == t2 && (t1 == Int || t1 == Bool || t2 == Str) then
        Right Bool
    else
        Left $ "Error, REL, " ++ (show e1) ++ (show e2)
getExprType (EAnd e1 e2) env = do
    t1 <- getExprType e1 env
    t2 <- getExprType e2 env
    if t1 == t2 && t1 == Bool then
        Right Bool
    else
        Left $ "Error, AND, " ++ (show e1) ++ (show e2)
getExprType (EOr e1 e2) env = do
    t1 <- getExprType e1 env
    t2 <- getExprType e2 env
    if t1 == t2 && t1 == Bool then
        Right Bool
    else
        Left $ "Error, OR, " ++ (show e1) ++ (show e2)

getExprType (EApp ef args) env = do
    ft <- getExprType ef env
    case ft of
        FType rt argt -> getReturnType rt argt
        FVType argt -> getReturnType None argt
        BuiltinFuncType id ->
            case id of
                "print" ->
                    case typeCheckExprs args of
                        Left s -> Left $ "Print param error, " ++ s
                        Right _ -> Right None
                "assert" ->
                    case L.length args of
                        1 -> getReturnType None [FTypeArg Bool]
                        2 -> getReturnType None [FTypeArg Bool, FTypeArg Str]
                        0 -> Left $ "@assert requires bool and optional string"
                "readLine" ->
                    getReturnType Str []
                "stringToInt" -> 
                    getReturnType Int [FTypeArg Str]
                "isNone" ->
                    if L.length args /= 1 then
                        Left "@isNone requires exactly 1 argument."
                    else
                        case getExprType (head args) env of
                            Left s -> Left $ "isNone param error, " ++ s
                            Right _ -> Right Bool
                "toString" ->
                    Right Str
        _ -> Left $ "Error, EAPP, " ++ (show ft)
    where
        getReturnType :: Type -> [FTypeArg] -> Either String Type
        getReturnType t argts =
            if checkArgTypes args argts then
                Right t
            else
                Left $ "Error, arg types, " ++ (show ef) ++"\n " ++ (show args) ++ "   " ++ (show argts)

        checkArgTypes :: [Expr] -> [FTypeArg] -> Bool
        checkArgTypes [] [] = True
        checkArgTypes _ [] = False
        checkArgTypes [] _ = False
        checkArgTypes (a:as) (t:ts) =
            case t of
                FTypeArg at -> checkArgType a at as ts
                FTypeArgRef at ->
                    case a of
                        (EIdent _) -> checkArgType a at as ts
                        _ -> False --Ref value should be identifier

        checkArgType a t as ts = do
            case getExprType a env of
                Left s -> False --Left $ "Error parsing argument, " ++ s
                Right at ->
                    if at == t then
                        checkArgTypes as ts
                    else
                        False --Left $ "Incorrect arg type, " ++ (show a)
        typeCheckExprs [] = Right ()
        typeCheckExprs (e:es) =
            case getExprType e env of
                Left s -> Left s
                Right _ -> typeCheckExprs es

getExprType (EStruct idDecl exprs) env =
    let strcts = structs env
        structName = extractIdentDecl idDecl
        structType = strcts HM.!? structName in
            case structType of
                Nothing -> Left "Struct undeclared"
                Just desc ->
                    if checkExprStructType (L.sortOn fst $ map snd $ HM.toList desc) exprs then
                        Right $ StructType idDecl
                    else
                        Left "Struct fields dont match"
    where
        checkExprStructType :: [StructFieldDesc] -> [Expr] -> Bool
        checkExprStructType [] [] = True
        checkExprStructType [] _ = False
        checkExprStructType _ [] = False
        checkExprStructType (fd:fds) (e:es) =
            let t = getExprType e env in
                if t == (Right $ snd fd) then
                    checkExprStructType fds es
                else
                    False

getExprType (EStructNamed idDecl fieldExprs) env =
    let strcts = structs env
        structName = extractIdentDecl idDecl
        structType = strcts HM.!? structName in
            case structType of
                Nothing -> Left "Struct undeclared"
                Just desc -> 
                    if evalExprStructNamed desc fieldExprs then
                        Right $ StructType idDecl
                    else
                        Left "Struct fields dont match"
    where
        evalExprStructNamed :: StructDescription -> [EStructNamedF] -> Bool
        evalExprStructNamed sd fs =
            let fnames = map (\(EStructNamedF id _) -> extractIdentDecl id) fs in
                if L.nub fnames == fnames then -- Dont allow to assign the same field twice
                    evalExprStructNamedHelper sd fs
                else
                    False
        evalExprStructNamedHelper :: StructDescription -> [EStructNamedF] -> Bool
        evalExprStructNamedHelper _ [] = True
        evalExprStructNamedHelper sd (EStructNamedF id expr : fs) =
            let fieldName = extractIdentDecl id
                fieldDesc = sd HM.!? fieldName in
                    case fieldDesc of
                        Nothing -> False
                        Just (_, t) -> 
                            let et = getExprType expr env in
                                if et == Right t then
                                    evalExprStructNamedHelper sd fs
                                else
                                    False

getExprType (EFunc t args stmts) env =
    case typeCheckStmtsRun stmts (addArgsToEnv args env) t of
        Left s -> Left s -- return value should be passed
        Right True -> Right $ FType t $ map (\(Arg _ atype) -> atype) args
        Right False -> Left "Function did not return any value"
getExprType (EVFunc args stmts) env =
    case typeCheckStmtsRun stmts (addArgsToEnv args env) None of
        Left s -> Left s -- return value should be passed
        Right _ -> Right $ FVType $ map (\(Arg _ atype) -> atype) args

addArgsToEnv :: [Arg] -> Env -> Env
addArgsToEnv [] env = env
addArgsToEnv (Arg idDecl (FTypeArg t):as) env =
    let vs = vars env
        newEnv = env { vars = HM.insert (extractIdentDecl idDecl) (Var { loc = 0, varType = t}) vs } in
    addArgsToEnv as newEnv
addArgsToEnv (Arg idDecl (FTypeArgRef t):as) env =
    let vs = vars env
        newEnv = env { vars = HM.insert (extractIdentDecl idDecl) (Var { loc = 0, varType = t}) vs } in
    addArgsToEnv as newEnv