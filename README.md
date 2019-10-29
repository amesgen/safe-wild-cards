# safe-wild-cards

`RecordWildCards` are convenient, but sometimes you want to assert that
you have handled all fields of a record.

Let's say you are writing a `ToJSON` instance for a record:

```haskell
instance ToJSON Rec where
  toJSON Rec{..} = ...
```

To get compilation warnings whenever you update `Rec` and forget to handle
the new field in `toJSON`, change the instance to look like this:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import SafeWildCards

instance ToJSON Rec where
  toJSON $(fields 'Rec) = ...
```

If you want to ignore a field, do it explicitly:

```haskell
instance ToJSON Rec where
  toJSON $(fields 'Rec) = ...
    where
      _ = recUnusedField  -- not used because XYZ
```

If you want expose fields from several records, you can prefix them to
avoid clashes:

```haskell
diff :: Rec -> Rec -> Text
diff $(fieldsPrefixed "a_" 'Rec) $(fieldsPrefixed "b_" 'Rec) = ...
```
