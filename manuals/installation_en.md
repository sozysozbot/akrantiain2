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
$ ./akrantiain2 samples/sample_lineparine.snoj < samples/input_sample/input_sample_lineparine.txt
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
$ akrantiain2 samples\sample_lineparine.snoj --file < samples\input_sample\input_sample_lineparine.txt
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
$ ./akrantiain2 samples/sample_lineparine.snoj < samples/input_sample/input_sample_lineparine.txt
```
