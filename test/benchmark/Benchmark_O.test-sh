// REQUIRES: OS=macosx
// REQUIRES: asserts
// REQUIRES: benchmark
// REQUIRES: CMAKE_GENERATOR=Ninja

// RUN: %Benchmark_O --list | %FileCheck %s --check-prefix LISTTAGS
// LISTTAGS: AngryPhonebook,[
// LISTTAGS-NOT: TestsUtils.BenchmarkCategory.
// LISTTAGS-SAME: String,
// LISTTAGS-SAME: ]

// RUN: %Benchmark_O AngryPhonebook --num-iters=1 \
// RUN:             | %FileCheck %s --check-prefix NUMITERS1
// NUMITERS1: AngryPhonebook,1
// NUMITERS1-NOT: 0,0,0,0,0

// Should run benchmark by name, even if its tags match the default skip-tags
// (unstable,skip). Ackermann is marked unstable
// RUN: %Benchmark_O Ackermann | %FileCheck %s --check-prefix NAMEDSKIP
// NAMEDSKIP: Ackermann

// RUN: %Benchmark_O --list --tags=Dictionary,Array \
// RUN:             | %FileCheck %s --check-prefix ANDTAGS
// ANDTAGS: TwoSum
// ANDTAGS-NOT: Array2D
// ANDTAGS-NOT: DictionarySwap

// RUN: %Benchmark_O --list --tags=algorithm --skip-tags=validation \
// RUN:             | %FileCheck %s --check-prefix TAGSANDSKIPTAGS
// TAGSANDSKIPTAGS: Ackermann
// TAGSANDSKIPTAGS: DictOfArraysToArrayOfDicts
// TAGSANDSKIPTAGS: Fibonacci
// TAGSANDSKIPTAGS: RomanNumbers

// RUN: %Benchmark_O  --list --tags=algorithm \
// RUN:               --skip-tags=validation,Dictionary,String \
// RUN:             | %FileCheck %s --check-prefix ORSKIPTAGS
// ORSKIPTAGS: Ackermann
// ORSKIPTAGS-NOT: DictOfArraysToArrayOfDicts
// ORSKIPTAGS: Fibonacci
// ORSKIPTAGS-NOT: RomanNumbers

// RUN: %Benchmark_O  --list | %FileCheck %s --check-prefix LISTPRECOMMIT
// LISTPRECOMMIT: #,Test,[Tags]
// LISTPRECOMMIT-NOT: Ackermann
// LISTPRECOMMIT: {{[0-9]+}},AngryPhonebook

// RUN: %Benchmark_O  --list --skip-tags= | %FileCheck %s --check-prefix LISTALL
// LISTALL: Ackermann
// LISTALL: AngryPhonebook
