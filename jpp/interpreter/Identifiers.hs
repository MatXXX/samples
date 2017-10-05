module Identifiers (
    extractIdentStructField,
    extractIdentDecl,
    extractIdentNoDecl,
    extractIdentAll,
)
where

import AbsGram

extractIdentStructField :: StructField -> String
extractIdentStructField (StructField id) = extractIdentDecl id

extractIdentDecl :: IdentDecl -> String
extractIdentDecl (IdentDecl (Ident id)) = id

extractIdentNoDecl :: IdentNoDecl -> String
extractIdentNoDecl (IdentBuiltin (Builtin id)) = id

extractIdentAll :: IdentAll -> String
extractIdentAll (IdentAllD id) = extractIdentDecl id
extractIdentAll (IdentAllBuiltin id) = extractIdentNoDecl id
