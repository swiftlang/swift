<!--
REQUIRES: OS=macosx
REQUIRES: asserts
REQUIRES: benchmark
REQUIRES: CMAKE_GENERATOR=Ninja
-->
# `Benchmark_O` Tests

The `Benchmark_O` binary is used directly from command line as well as a
subcomponent invoked from higher-level scripts (eg. [`Benchmark_Driver`][BD]).
These script therefore depend on the supported command line options and the
format of its console output. The following [`lit` tests][Testing] also serve
as a verification of this public API to prevent its accidental breakage.

[BD]: https://github.com/apple/swift/blob/master/benchmark/scripts/Benchmark_Driver
[Testing]: https://github.com/apple/swift/blob/master/docs/Testing.md

Note: Following tests use *Ackermann* as an example of a benchmark that is
excluded from the default "pre-commit" list because it is marked `unstable` and
the default skip-tags (`unstable,skip`) will exclude it. It's also
alphabetically the first benchmark in the test suite (used to verify running by
index). If these assumptions change, the test must be adapted.

## List Format
````
RUN: %Benchmark_O --list | %FileCheck %s \
RUN:                      --check-prefix LISTPRECOMMIT \
RUN:                      --check-prefix LISTTAGS
LISTPRECOMMIT: #,Test,[Tags]
LISTPRECOMMIT-NOT: Ackermann
LISTPRECOMMIT: {{[0-9]+}},AngryPhonebook
LISTTAGS-SAME: ,[
LISTTAGS-NOT: TestsUtils.BenchmarkCategory.
LISTTAGS-SAME: String, api, validation
LISTTAGS-SAME: ]
````

Verify Ackermann is listed when skip-tags are explicitly empty and that it is
marked unstable:

````
RUN: %Benchmark_O --list --skip-tags= | %FileCheck %s --check-prefix LISTALL
LISTALL: Ackermann
LISTALL-SAME: unstable
LISTALL: AngryPhonebook
````

## Benchmark Selection
The logic for filtering tests based on specified names, indices and tags
is shared between the default "run" and `--list` commands. It is tested on
the list command, which is much faster, because it runs no benchmarks.
It provides us with ability to do a "dry run".

Run benchmark by name (even if its tags match the skip-tags) or test number:

````
RUN: %Benchmark_O Ackermann --list | %FileCheck %s --check-prefix NAMEDSKIP
NAMEDSKIP: Ackermann

RUN: %Benchmark_O 1 --list | %FileCheck %s --check-prefix RUNBYNUMBER
RUNBYNUMBER: Ackermann
````

Composition of `tags` and `skip-tags`:

````
RUN: %Benchmark_O --list --tags=Dictionary,Array \
RUN:             | %FileCheck %s --check-prefix ANDTAGS
ANDTAGS: TwoSum
ANDTAGS-NOT: Array2D
ANDTAGS-NOT: DictionarySwap

RUN: %Benchmark_O --list --tags=algorithm --skip-tags=validation \
RUN:             | %FileCheck %s --check-prefix TAGSANDSKIPTAGS
TAGSANDSKIPTAGS: Ackermann
TAGSANDSKIPTAGS: DictOfArraysToArrayOfDicts
TAGSANDSKIPTAGS: Fibonacci
TAGSANDSKIPTAGS: RomanNumbers

RUN: %Benchmark_O --list --tags=algorithm \
RUN:              --skip-tags=validation,Dictionary,String \
RUN:             | %FileCheck %s --check-prefix ORSKIPTAGS
ORSKIPTAGS: Ackermann
ORSKIPTAGS-NOT: DictOfArraysToArrayOfDicts
ORSKIPTAGS: Fibonacci
ORSKIPTAGS-NOT: RomanNumbers
````

## Running Benchmarks
Each real benchmark execution takes about a second per sample. If possible,
multiple checks are combined into one run to minimize the test time.

````
RUN: %Benchmark_O AngryPhonebook --num-iters=1 \
RUN:              | %FileCheck %s --check-prefix NUMITERS1 \
RUN:                              --check-prefix LOGHEADER \
RUN:                              --check-prefix LOGBENCH
LOGHEADER-LABEL: #,TEST,SAMPLES,MIN(us),MAX(us),MEAN(us),SD(us),MEDIAN(us)
LOGBENCH: {{[0-9]+}},
NUMITERS1: AngryPhonebook,1
NUMITERS1-NOT: 0,0,0,0,0
LOGBENCH-SAME: ,{{[0-9]+}},{{[0-9]+}},{{[0-9]+}},{{[0-9]+}},{{[0-9]+}}
````

### Verbose Mode

````
RUN: %Benchmark_O 1 Ackermann 1 AngryPhonebook \
RUN:              --verbose --num-samples=2 --memory \
RUN:              | %FileCheck %s --check-prefix RUNJUSTONCE \
RUN:                              --check-prefix CONFIG \
RUN:                              --check-prefix LOGVERBOSE \
RUN:                              --check-prefix MEASUREENV \
RUN:                              --check-prefix LOGMEMORY
CONFIG: NumSamples: 2
CONFIG: Tests Filter: ["1", "Ackermann", "1", "AngryPhonebook"]
CONFIG: Tests to run: Ackermann, AngryPhonebook
LOGMEMORY: #,TEST,SAMPLES,MIN(us),MAX(us),MEAN(us),SD(us),MEDIAN(us),MAX_RSS(B)
LOGVERBOSE-LABEL: Running Ackermann for 2 samples.
LOGVERBOSE: Measuring with scale {{[0-9]+}}.
LOGVERBOSE-NEXT: Sample 0,{{[0-9]+}}
LOGVERBOSE-NEXT: Measuring with scale {{[0-9]+}}.
LOGVERBOSE-NEXT: Sample 1,{{[0-9]+}}
MEASUREENV: MAX_RSS {{[0-9]+}} - {{[0-9]+}} = {{[0-9]+}} ({{[0-9]+}} pages)
MEASUREENV: ICS {{[0-9]+}} - {{[0-9]+}} = {{[0-9]+}}
MEASUREENV: VCS {{[0-9]+}} - {{[0-9]+}} = {{[0-9]+}}
RUNJUSTONCE-LABEL: 1,Ackermann
RUNJUSTONCE-NOT: 1,Ackermann
LOGMEMORY: ,{{[0-9]+}},{{[0-9]+}},{{[0-9]+}},{{[0-9]+}},{{[0-9]+}},{{[0-9]+}}
LOGVERBOSE-LABEL: Running AngryPhonebook for 2 samples.
````

## Error Handling

````
RUN: not %Benchmark_O --bogus 2>&1 \
RUN:              | %FileCheck %s --check-prefix ARGPARSE
ARGPARSE: error: unsupported argument '--bogus'

RUN: not %Benchmark_O --sample-time \
RUN:         2>&1 | %FileCheck %s --check-prefix NOVALUE
NOVALUE: error: missing value for '--sample-time'

RUN: not %Benchmark_O --sample-time= \
RUN:         2>&1 | %FileCheck %s --check-prefix EMPTYVAL
EMPTYVAL: error: missing value for '--sample-time'

RUN: not %Benchmark_O --sample-time=NaN \
RUN:         2>&1 | %FileCheck %s --check-prefix NANVALUE
NANVALUE: error: 'NaN' is not a valid 'Double' for '--sample-time'

RUN: not %Benchmark_O --num-iters \
RUN:         2>&1 | %FileCheck %s --check-prefix NUMITERS
NUMITERS: error: missing value for '--num-iters'

RUN: not %Benchmark_O --num-samples \
RUN:         2>&1 | %FileCheck %s --check-prefix NUMSAMPLES
NUMSAMPLES: error: missing value for '--num-samples'

RUN: not %Benchmark_O --sleep \
RUN:         2>&1 | %FileCheck %s --check-prefix SLEEP
SLEEP: error: missing value for '--sleep'

RUN: not %Benchmark_O --delim \
RUN:         2>&1 | %FileCheck %s --check-prefix DELIM
DELIM: error: missing value for '--delim'

RUN: not %Benchmark_O --tags=bogus \
RUN:         2>&1 | %FileCheck %s --check-prefix BADTAG
BADTAG: error: 'bogus' is not a valid 'BenchmarkCategory'

RUN: not %Benchmark_O --skip-tags=bogus \
RUN:         2>&1 | %FileCheck %s --check-prefix BADSKIPTAG
BADSKIPTAG: error: 'bogus' is not a valid 'BenchmarkCategory'

````

## Usage

````
RUN: %Benchmark_O --help | %FileCheck %s --check-prefix OPTIONS
OPTIONS: usage: Benchmark_O [--argument=VALUE] [TEST [TEST ...]]
OPTIONS: optional arguments:
OPTIONS: --help
OPTIONS-SAME: show this help message and exit
OPTIONS: --verbose
OPTIONS: --delim
OPTIONS: --tags
OPTIONS: --list
````
