エラーや警告の一覧。構文エラー・意味論的エラー・意味論的警告・モジュールエラー・モジュール警告・実行時エラーの6種類がある。  
構文エラー以外のメッセージには一意な番号が振られている。

# 意味論的メッセージ

## 意味論的エラー
errNum | errStr | 意味
----- | ----- | -----
`333`|`"mismatched number of concrete terms in left- and right-hand side of:\n" ++ toSource conv ++ "\nleft: " ++ show(length[()｜Right _ <- midd']) ++ "; right: " ++ show(length phonemes)` | 変換規則定義文で、左右の具体項の個数が異なっている
`334`|`"duplicate definition regarding identifier(s) {" ++ intercalate "}, {" (map unId duplicates) ++ "}"` | 同一の識別子が複数回、識別子定義文で定義されている 
`335`|`"unresolved identifier {" ++ unId iden ++ "}"` | 識別子定義文で定義されていない識別子が使用されている
`336`|`"right-hand side of the following sentence consists solely of dollar(s):\n" ++ toSource conv` | 変換規則定義文で、右側が全て`$`である

## 意味論的警告
warnNum | warnStr | 意味
----- | ----- | ---
`2435`|`"unknown setting-specifier(s) " ++ toBraces unknowns` | 未知の環境指定識別子がある



# モジュールメッセージ

## モジュールエラー
errorNo | errorMsg | 意味
----- | ----- | ---
`1111`|`"Module {" ++ toSource name ++ "} does not exist"`  
`1112`|`"Circular reference involving module {" ++ toSource name ++ "}"`  
`1523`|`"Duplicate definition of module(s) "++str` | 

## モジュール警告
warningNo | warningMsg | 意味
----- | ----- | ---
`2000`|`"Unused module(s) " ++ toBraces (S.toList unused)` | 定義されているが使用されていないモジュールがある



# 実行時メッセージ

## 実行時エラー
errNo | errMsg | 意味
----- | ----- | ---
`210`|`"no rules that can handle character(s) "++ msg` | 変換できない文字が入力にある

