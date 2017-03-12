# akrantiain
A domain-specific language designed to describe conlangs' orthographies

## Documentations
[English manual](https://sozysozbot.github.io/akrantiain2/manuals/manuals_en.htm) (Incomplete)  
[日本語マニュアル](https://sozysozbot.github.io/akrantiain2/manuals/manuals_ja.htm) (未完)
## Getting Started

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

Or use it interactively:
```
$ ./akrantiain2 samples/sample_lineparine.snoj
```

Or read from file:
```
$ ./akrantiain2 samples/sample_lineparine.snoj < input_sample.txt
```

#### Windows (7 and 10)
1. Install Haskell Platform (>= 7.10.2).
2. Clone the repo.
3. Run:
```
$ ghc --make akrantiain2.hs
```

Try it out by running:
```
$ echo lineparine | akrantiain2 samples\sample_lineparine.snoj
```

Or use it interactively:
```
$ akrantiain2 samples\sample_lineparine.snoj
```

Or read from file:
```
$ chcp 65001
$ akrantiain2 samples\sample_lineparine.snoj --file < input_sample.txt
```
