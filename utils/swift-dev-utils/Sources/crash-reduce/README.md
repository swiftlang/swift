# crash-reduce

Takes a set of crashing Swift source files, and attempts to produce reduced test cases suitable for adding to the test suite. A simple invocation looks like:

```
./utils/crash-reduce --swift-path /path/to/swift <inputs> ...
```

You can either pass the input files directly as arguments, or use `-f` to pass a directory containing the inputs. By default the test cases are added to `validation-test/compiler_crashers` and `validation-test/IDE/crashers`, you can pass `-t`/`--ide-crashers` to customize this behavior. Note that the Swift path must currently be for a locally-built compiler since the tool also relies on `swift-ide-test`.

```
OVERVIEW: Reduce crashers into reproducer test cases

USAGE: crash-reduce reduce [-f <f>] [-t <t>] [--ide-crashers <ide-crashers>] --swift-path <swift-path> [--sdk-path <sdk-path>] [--reprocess] [--ignore-existing] [--quick] [--rm] [<inputs> ...]

ARGUMENTS:
  <inputs>                Path(s) to crashers; use '-f' for a directory of crashers

OPTIONS:
  -f <f>                  Path to a directory containing crashers
  -t <t>                  The target directory to place the reduced crashers
  --ide-crashers <ide-crashers>
                          The directory to place IDE crashers.
  --swift-path <swift-path>
                          Path to the Swift compiler to use
  --sdk-path <sdk-path>   Path to the SDK to use
  --reprocess             Reprocess all the reproducers in the target directory
  --ignore-existing       Ignore any existing reproducers in the target directory, producing new
                          reproducers
  --quick                 Avoid trying reproducer configurations that are slow
  --rm                    Delete input files on successful reduction
  -h, --help              Show help information.
```
