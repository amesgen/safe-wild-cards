-- | Use @RecordWildCards@ safely.
module SafeWildCards (fields, fieldsPrefixed) where

import Language.Haskell.TH (Name, PatQ, mkName, conP, varP, nameBase)
import Language.Haskell.TH.Datatype

-- | Put all fields of a record constructor into scope.
--
-- @f $(fields 'Rec) = ...@ is equivalent to @f Rec{..}@, but the compiler
-- will warn you on all unused fields. Thus 'fields' brings safety whenever
-- you want to guarantee that a certain function uses all fields of @Rec@.
--
-- To explicitly ignore a field, match it against @_@:
--
-- @
-- f $(fields 'Rec) = ...
--   where
--     -- Ignored fields
--     _ = (recUselessField1, recUselessField2)
-- @
--
-- Usage examples include @ToJSON@ instances and various encoders in
-- general:
--
-- @
-- instance ToJSON Rec where
--   toJSON $(fields 'Rec) = ...
-- @
fields :: Name -> PatQ
fields recordConstructor = do
  cons <- reifyConstructor recordConstructor
  case constructorVariant cons of
    RecordConstructor recordFields ->
      conP recordConstructor (map (varP . mkName . nameBase) recordFields)
    _ -> fail $
      "Expected " ++ show recordConstructor ++ " to be a record constructor"

-- | Like 'fields', but prefixes all fields with the given prefix.
--
-- Useful if you need to put fields from more than one record into scope.
fieldsPrefixed :: String -> Name -> PatQ
fieldsPrefixed prefix recordConstructor = do
  cons <- reifyConstructor recordConstructor
  case constructorVariant cons of
    RecordConstructor recordFields ->
      conP recordConstructor (map (varP . mkName . (prefix <>) . nameBase) recordFields)
    _ -> fail $
      "Expected " ++ show recordConstructor ++ " to be a record constructor"
