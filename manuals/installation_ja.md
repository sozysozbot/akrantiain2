akrantiainで扱う全てのファイルはUTF-8である必要があります。

### インストール法
#### Ubuntu (16.04)
リポジトリをクローンして、
```
$ make
```
を実行するとインストールできます。

akrantiainを試用するには、
```
$ echo lineparine | ./akrantiain2 samples/sample_lineparine.snoj
```
を実行してみましょう。

```
$ ./akrantiain2 samples/sample_lineparine.snoj
```
を実行するとインタラクティブに実行できます。([Ctrl+D] -> [Enter] で終了できます)

ファイルから読むなら、
```
$ ./akrantiain2 samples/sample_lineparine.snoj < samples/input_sample/input_sample_lineparine.txt
```
と実行することでできます。

#### Windows (2020年6月24日に検証)
1. [ここ](https://docs.haskellstack.org/en/stable/README/) の 「Windows 64-bit Installer」の箇所をクリックしてインストール
2. コマンドプロンプトで `stack setup`
3. 待つ
4. (検証中)


#### Windows (7 and 10) (以下の情報は2017年8月に検証したものなので古いです）
1. Haskell Platform Full (>= 7.10.2)をインストール
2. リポジトリをクローン
3. `make.bat`をダブルクリック

akrantiainを試用するには、
```
$ echo lineparine | akrantiain2 samples\sample_lineparine.snoj
```
を実行してみましょう。

```
$ akrantiain2 samples\sample_lineparine.snoj
```
を実行するとインタラクティブに実行できます。([Ctrl+Z] -> [Enter] で終了できます)

ファイルから読むなら、
```
$ chcp 65001
$ akrantiain2 samples\sample_lineparine.snoj --file < samples\input_sample\input_sample_lineparine.txt
```
と実行することでできます。

注意: Windowsでは、入力に多バイト文字が含まれるときはインタラクティブ実行が動作しません。  
「ファイルから読み込む」モードでakrantiainを実行することで正しく動作します。

#### Mac
1. Haskell Platform Full (>= 7.10.2)をインストール
2. リポジトリをクローン
3. 
```
$ make
```
を実行

akrantiainを試用するには、
```
$ echo lineparine | ./akrantiain2 samples/sample_lineparine.snoj
```
を実行してみましょう。

```
$ ./akrantiain2 samples/sample_lineparine.snoj
```
を実行するとインタラクティブに実行できます。(control+Dで終了できます)

ファイルから読むなら、
```
$ ./akrantiain2 samples/sample_lineparine.snoj < samples/input_sample/input_sample_lineparine.txt
```
と実行することでできます。
