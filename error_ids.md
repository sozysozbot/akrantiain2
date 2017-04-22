## SemanticError
`Left E{errNum = 333, errStr = "mismatched number of concrete terms in left- and right-hand side of:\n" ++ toSource conv}`  
`Left E{errNum = 334, errStr = "duplicate definition regarding identifier(s) {" ++ intercalate "}, {" (map unId duplicates) ++ "}"}`   
`Left E{errNum = 335, errStr = "unresolved identifier {" ++ unId iden ++ "}"}`  

## RuntimeError
`Left RE{errNo = 210, errMsg = "no rules that can handle character(s) "++ msg}`

## ModuleError
`Left $ ME {errorNo = 1111, errorMsg = "Module {" ++ toSource name ++ "} does not exist"}`  
`Left $ ME {errorNo = 1112, errorMsg = "Circular reference involving module {" ++ toSource name ++ "}"}`  
`Left $ ME {errorNo = 1523, errorMsg = "Duplicate definition of module(s) "++str}`  
