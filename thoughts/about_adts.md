```

this redundance on ADTs is something rust solves well

data Declaration
  = DeclType            TypeDeclaration
  | DeclFunction        FunctionDeclaration
  | DeclTypeSignature   TypeSignature
...

newtype TypeDeclaration = TypeDeclaration
  { name          :: String
  , typeSignature :: Maybe TypeSignature
  }

newtype FunctionDeclaration = FunctionDeclaration
  { name          :: String
  , parameters    :: Array Identifier
  , body          :: Expression
  , typeSignature :: Maybe TypeSignature
  }```

```
type Declaration {
   TypeDeclaration { name          :: String
  , typeSignature :: Maybe TypeSignature
  }
  FunctionDeclaration { name          :: String
  , parameters    :: Array Identifier
  , body          :: Expression
  , typeSignature :: Maybe TypeSignature
  }
}
... etc
```
