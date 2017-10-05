module InterpreterState (
    Location,
    Variable (Var, loc, varType),
    StructDescription,
    StructFieldDesc,
    StructValue,
    StructsDict,
    Env (Env, vars, structs),
    Storage,
    Closure,
    ExprValue (ExprInt, ExprBool, ExprString, ExprFunc, ExprStruct, ExprNone, ExprBuiltinFunc),
    InterpreterState,
    addVariable,
    addVariableRef,
    addStruct,
    setReference,
    setFieldValue,
    getStructField,
    setVariable
) where

import AbsGram

import qualified Data.Map.Strict as HM
import qualified Data.Sequence as SQ

import Control.Monad.State
import Control.Monad.Except

import Identifiers

type Location = Int
data Variable = Var { loc :: Location, varType :: Type } deriving (Show)
type StructDescription = HM.Map String StructFieldDesc -- Int -> location in StructValue
type StructFieldDesc = (Int, Type)
type StructValue = SQ.Seq ExprValue
type StructsDict = HM.Map String StructDescription
data Env = Env { vars :: HM.Map String Variable, structs :: StructsDict } deriving (Show)
type Storage = SQ.Seq ExprValue
type Closure = Env --Closure { env :: Env, storage :: Storage } deriving (Show)
data ExprValue = ExprInt Int
               | ExprBool Bool
               | ExprString String
               | ExprFunc (Maybe Type) [Arg] [Stmt] Closure
               | ExprStruct String StructValue
               | ExprNone
               | ExprBuiltinFunc String
               deriving (Show)

type InterpreterState = StateT (Env, Storage) (ExceptT String IO) (Maybe ExprValue)

addVariable :: String -> Type -> ExprValue -> InterpreterState
addVariable id t e = do
    (env, st) <- get
    let hm = vars env
        loc = fromIntegral $ length st
        hm2 = HM.insert id Var { loc = loc, varType = t } hm
        st2 = st SQ.|> e
        env2 = env { vars = hm2 } in
        put (env2, st2);
        return Nothing

addVariableRef :: String -> Type -> InterpreterState
addVariableRef id t = do
    (env, st) <- get
    let hm = vars env
        hm2 = HM.insert id Var { loc = -1, varType = t } hm
        env2 = env { vars = hm2 } in
        put (env2, st);
        return Nothing

setReference :: String -> Expr -> Env -> InterpreterState
setReference newId (EIdent oldIdAll) oldEnv = do
    (env, st) <- get;
    let oldId = extractIdentAll oldIdAll
        hm = vars oldEnv
        var = hm HM.! oldId
        hm2 = HM.insert newId var (vars env)
        env2 = env { vars = hm2 } in do
            put (env2, st);
            return Nothing

addStruct :: String -> StructDescription -> InterpreterState
addStruct name desc = do
    (env, st) <- get
    put (env { structs = HM.insert name desc (structs env) }, st)
    return Nothing

setFieldValue :: StructsDict -> Type -> ExprValue -> [StructField] -> ExprValue -> ExprValue
setFieldValue structs _ (ExprStruct name values) [field] val = 
    let structDesc = structs HM.! name
        (loc, _) = structDesc HM.! extractIdentStructField field in
            ExprStruct name (SQ.update loc val values)
        
setFieldValue structs t (ExprStruct name values) (f:fs) val =
    let structDesc = structs HM.! name
        (loc, _) = structDesc HM.! extractIdentStructField f
        newValue = setFieldValue structs t (SQ.index values loc) fs val in
            ExprStruct name (SQ.update loc newValue values)

setFieldValue structs t@(StructType idDecl) ExprNone fs val =
    let name = extractIdentDecl idDecl in
        setFieldValue structs t (createEmptyStruct structs name) fs val

createEmptyStruct :: StructsDict -> String -> ExprValue
createEmptyStruct structs name =
    let structDesc = structs HM.! name
        structSize = HM.size structDesc in
            ExprStruct name (SQ.replicate structSize ExprNone)

getStructField :: StructsDict -> ExprValue -> [StructField] -> ExprValue
getStructField structs (ExprStruct name values) [field] = 
    let structDesc = structs HM.! name
        (loc, _) = structDesc HM.! extractIdentStructField field in
            SQ.index values loc

getStructField structs (ExprStruct name values) (f:fs) =
    let structDesc = structs HM.! name
        (loc, _) = structDesc HM.! extractIdentStructField f in
            getStructField structs (SQ.index values loc) fs

setVariable :: String -> ExprValue -> InterpreterState
setVariable id e = do
    (env, st) <- get
    let hm = vars env
        Var loc t = hm HM.! id
        st2 = SQ.update loc e st
        hm2 = HM.insert id (Var loc t) hm in
            put (env { vars = hm2 }, st2);
            return Nothing