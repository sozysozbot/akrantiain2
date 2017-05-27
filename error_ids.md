# Semantic Messages

## SemanticError
errNum | errStr
----- | -----
`333`|`"mismatched number of concrete terms in left- and right-hand side of:\n" ++ toSource conv ++ "\nleft: " ++ show(length[()｜Right _ <- midd']) ++ "; right: " ++ show(length phonemes)`  
`334`|`"duplicate definition regarding identifier(s) {" ++ intercalate "}, {" (map unId duplicates) ++ "}"`   
`335`|`"unresolved identifier {" ++ unId iden ++ "}"`  
`336`|`"right-hand side of the following sentence consists solely of dollar(s):\n" ++ toSource conv`

## SemanticWarning
warnNum | warnStr
----- | -----
`2435`|`"unknown setting-specifier(s) " ++ toBraces unknowns`



# Module Messages

## ModuleError
errorNo | errorMsg
----- | -----
`1111`|`"Module {" ++ toSource name ++ "} does not exist"`  
`1112`|`"Circular reference involving module {" ++ toSource name ++ "}"`  
`1113`|`"Duplicate definition of module(s) "++str`  

## ModuleWarning
warningNo | warningMsg 
----- | -----
`2000`|`"Unused module(s) " ++ toBraces (S.toList unused)`



# Runtime Messages

## RuntimeError
errNo | errMsg
----- | -----
`210`|`"no rules that can handle character(s) "++ msg`

