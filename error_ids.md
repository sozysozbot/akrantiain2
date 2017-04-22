## SemanticError
errNum | errStr
----- | -----
`333`|`"mismatched number of concrete terms in left- and right-hand side of:\n" ++ toSource conv`  
`334`|`"duplicate definition regarding identifier(s) {" ++ intercalate "}, {" (map unId duplicates) ++ "}"`   
`335`|`"unresolved identifier {" ++ unId iden ++ "}"`  

## RuntimeError
errNo | errMsg
----- | -----
`210`|`"no rules that can handle character(s) "++ msg`

## ModuleError
errorNo | errorMsg
----- | -----
`1111`|`"Module {" ++ toSource name ++ "} does not exist"`  
`1112`|`"Circular reference involving module {" ++ toSource name ++ "}"`  
`1523`|`"Duplicate definition of module(s) "++str`  
