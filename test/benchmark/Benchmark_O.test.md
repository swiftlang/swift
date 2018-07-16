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

````
RUN: %Benchmark_O AngryPhonebook --num-iters=1 \
RUN:             | %FileCheck %s --check-prefix NUMITERS1
NUMITERS1: AngryPhonebook,1
NUMITERS1-NOT: 0,0,0,0,0
````

Should run benchmark by name, even if its tags match the default skip-tags
(unstable,skip). Ackermann is marked unstable

````
RUN: %Benchmark_O Ackermann | %FileCheck %s --check-prefix NAMEDSKIP
NAMEDSKIP: Ackermann

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
