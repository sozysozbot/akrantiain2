エラーや警告の一覧。構文エラー・意味論的エラー・意味論的警告・モジュールエラー・モジュール警告・実行時エラーの6種類がある。  
構文エラー以外のメッセージには一意な番号が振られている。

# 意味論的メッセージ

## 意味論的エラー
errNum | errStr | 意味
----- | ----- | -----
`333`|`"mismatched number of concrete terms in left- and right-hand side of:\n" ++ toSource conv ++ "\nleft: " ++ show(length[()｜Right _ <- midd']) ++ "; right: " ++ show(length phonemes)` | 変換規則定義文で、左右の具体項の個数が異なっている
`334`|`"duplicate definition regarding identifier(s) {" ++ intercalate "}, {" (map unId duplicates) ++ "}"` | 同一名の識別子が複数回、識別子定義文で定義されている 
`335`|`"unresolved identifier {" ++ unId iden ++ "}"` | 識別子定義文で定義されていない識別子が使用されている
`336`|`"right-hand side of the following sentence consists solely of dollar(s):\n" ++ toSource conv` | 変換規則定義文で、右側が全て`$`である
`337`|`"a punctuation or space found inside a pattern string(s) "++toBraces (map Quote illegals)`|パターン文字列の中にスペースか句読点が入っている（注：語境界にマッチするには`^`を用いなければならない）

## 意味論的警告
warnNum | warnStr | 意味
----- | ----- | ---
`2435`|`"unknown setting-specifier(s) " ++ toBraces unknowns` | 未知の環境指定識別子がある



# モジュールメッセージ

## モジュールエラー
errorNo | errorMsg | 意味
----- | ----- | ---
`1111`|`"Module {" ++ toSource name ++ "} does not exist"` | 定義されていないモジュールが使用されている
`1112`|`"Circular reference involving module {" ++ toSource name ++ "}"` | モジュールが循環依存を起こしている
`1113`|`"Duplicate definition of module(s) "++str` | 同一名のモジュールが複数回定義されている

## モジュール警告
warningNo | warningMsg | 意味
----- | ----- | ---
`2000`|`"Unused module(s) " ++ toBraces (S.toList unused)` | 定義されているが使用されていないモジュールがある



# 実行時メッセージ

## 実行時エラー
errNo | errMsg | 意味
----- | ----- | ---
`210`|`"no rules that can handle character(s) "++ msg` | 変換できない文字が入力にある

