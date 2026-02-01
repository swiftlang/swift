# crash-reduce

Takes a set of crashing Swift source files, and attempts to produce reduced test cases suitable for adding to the test suite. A simple invocation looks like:

```
./utils/crash-reduce --swift-path /path/to/swift <inputs>...
```

You can either pass the input files directly as arguments, or use `-f` to pass a directory containing the inputs. By default the test cases are added to `validation-test/compiler_crashers` and `validation-test/IDE/crashers`, you can pass `-t`/`--ide-crashers` to customize this behavior. By default the crash signatures are coalesced such that only new signatures are added, you can pass `--ignore-existing` to override this.

By default `crash-reduce` tries a few different frontend invocations to reproduce the crash, if you want to pass an exact set of frontend arguments you can do so after `--`:

```
./utils/crash-reduce --swift-path /path/to/swift <inputs>... -- -typecheck -enable-experimental-feature SomeFeature
```

## Usage

```
USAGE: crash-reduce reduce [-f <f>] [-t <t>] [--ide-crashers <ide-crashers>] --swift-path <swift-path> [--sdk-path <sdk-path>] [--reprocess] [--ignore-existing] [--quick] [--rm] [<inputs> ...] -- [<frontend-args> ...]

ARGUMENTS:
  <inputs>                Path(s) to crashers; use '-f' for a directory of crashers
  <frontend-args>         A set of frontend arguments to use for reproducing a frontend crash. If
                          not provided, a default set of arguments will be used.

OPTIONS:
  -f <f>                  Path to a directory containing crashers
  -t <t>                  The target directory to place the reduced crashers
  --ide-crashers <ide-crashers>
                          The directory to place IDE crashers.
  --swift-path <swift-path>
                          Path to the Swift compiler to use
  --sdk-path <sdk-path>   Path to the SDK to use. If not set it will be inferred.
  --reprocess             Reprocess all the reproducers in the target directory
  --ignore-existing       Ignore any existing reproducers in the target directory, producing new
                          reproducers
  --quick                 Avoid trying reproducer configurations that are slow
  --rm                    Delete input files on successful reduction
  -h, --help              Show help information.
```
