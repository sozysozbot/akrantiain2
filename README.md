# akrantiain
A domain-specific language designed to describe conlangs' orthographies

## Documentations
[English manual about sentence sequences](https://sozysozbot.github.io/akrantiain2/manuals/manuals_en.htm) (Incomplete) 

[概略](https://sozysozbot.github.io/akrantiain2/manuals/introduction/akrantiain.html)  
[概略(外部サイト)](http://ja.conlinguistics.wikia.com/wiki/Akrantiain)  

[仕様書トップページ](https://sozysozbot.github.io/akrantiain2/manuals/main_ja.htm)
- [.snojファイルとモジュールについての仕様書](https://sozysozbot.github.io/akrantiain2/manuals/modules_ja.htm)   
- [文列についての仕様書](https://sozysozbot.github.io/akrantiain2/manuals/manuals_ja.htm) (未完)  
- [akrantiain変換部についての仕様書](https://sozysozbot.github.io/akrantiain2/manuals/conversions_ja.htm)  
- [.snojファイルのjsonダンプの仕様](https://sozysozbot.github.io/akrantiain2/manuals/snoj_json_ja.html)

[Twitterで飛んできたQ&A](https://sozysozbot.github.io/akrantiain2/manuals/FAQ_ja.htm)(未完)  
[Togetter(外部サイト)](https://togetter.com/li/1131468)  
[メッセージと番号と内容の対応表](https://github.com/sozysozbot/akrantiain2/blob/master/manuals/error_ids_ja.md)  
ny lirsesen [kanteluescosti virle lineparine](https://sozysozbot.github.io/akrantiain2/manuals/manuals_conlang_lpa.htm)  

## Dreams and Reality
[現状](https://sozysozbot.github.io/akrantiain2/samples/current_situation.htm)

## Getting Started

All files handled by akrantiain must be in UTF-8.

### Installing
#### Ubuntu (16.04)
Clone the repo and run:
```
$ make
```

Try it out by running:
```
$ echo lineparine | ./akrantiain2 samples/sample_lineparine.snoj
```

Or use it interactively (press [Ctrl+D] -> [Enter] to terminate):
```
$ ./akrantiain2 samples/sample_lineparine.snoj
```

Or read from file:
```
$ ./akrantiain2 samples/sample_lineparine.snoj < samples/input_sample_lineparine.txt
```

#### Windows (7 and 10)
1. Install Haskell Platform Full (>= 7.10.2).
2. Clone the repo.
3. Double-click `make.bat`.

Try it out by running:
```
$ echo lineparine | akrantiain2 samples\sample_lineparine.snoj
```

Or use it interactively (press [Ctrl+Z] -> [Enter] to terminate):
```
$ akrantiain2 samples\sample_lineparine.snoj
```

Or read from file:
```
$ chcp 65001
$ akrantiain2 samples\sample_lineparine.snoj --file < samples\input_sample_lineparine.txt
```

Note: In Windows, interactive mode does not work when the input contains multi-byte characters.
In such cases, use "read from file" mode for akrantiain so that it can function properly.

#### Mac
1. Install Haskell Platform Full (>= 7.10.2).
2. Clone the repo.
3. Run:
```
$ make
```

Try it out by running:
```
$ echo lineparine | ./akrantiain2 samples/sample_lineparine.snoj
```

Or use it interactively (Press control+D to terminate):
```
$ ./akrantiain2 samples/sample_lineparine.snoj
```

Or read from file:
```
$ ./akrantiain2 samples/sample_lineparine.snoj < samples/input_sample_lineparine.txt
```
