-- | Use @-XRecordWildCards@ safely.
module SafeWildCards (fields, fieldsPrefixed, fieldsNamed) where

import Language.Haskell.TH (Name, PatQ, mkName, conP, varP, nameBase, recover)
import Language.Haskell.TH.Datatype

-- | Put all fields of a record constructor into scope.
--
-- @f $(fields 'Rec) = ...@ is equivalent to @f Rec{..}@, but the compiler
-- will warn you about all unused fields. Thus 'fields' brings compile-time
-- safety whenever you want to guarantee that a certain function uses all
-- fields of @Rec@.
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
fields = fieldsNamed id

-- | Like 'fields', but prefixes all fields with the given prefix.
--
-- Useful if you need to put fields from more than one record into scope:
--
-- @
-- diff :: Rec -> Rec -> Text
-- diff $(fieldsPrefixed "a_" 'Rec) $(fieldsPrefixed "b_" 'Rec) = ...
-- @
fieldsPrefixed :: String -> Name -> PatQ
fieldsPrefixed prefix = fieldsNamed (prefix ++)

-- | General form of 'fields' and 'fieldsPrefixed'.
fieldsNamed :: (String -> String) -> Name -> PatQ
fieldsNamed f recordConstructor = do
  cons <-
    recover
      (fail $
         "Could not find " ++ nameBase recordConstructor ++ ". If it is defined in the same module where you are using\n" ++
         "      'safe-wild-cards', you need to break the declaration group like this:\n" ++
         "\n" ++
         "          data ... = " ++ nameBase recordConstructor ++ " ...\n" ++
         "          $(pure [])\n" ++
         "\n" ++
         "      Read the 'SafeWildCards' module documentation for more details.\n"
      )
      (reifyConstructor recordConstructor)
  case constructorVariant cons of
    RecordConstructor recordFields ->
      conP recordConstructor (map (varP . mkName . f . nameBase) recordFields)
    _ -> fail $
      "Expected " ++ show recordConstructor ++ " to be a record constructor"
