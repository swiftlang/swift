// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t && cp %t/fileB{0-baseline,}.swift

// =============================================================================
// Without range dependencies, but logging comparison
// =============================================================================

// RUN: cd %t && %swiftc_driver -enable-batch-mode -j2 -incremental  -driver-show-incremental  ./main.swift ./fileA.swift ./fileB.swift -module-name main -output-file-map %t/output.json  >& output1

// =============================================================================
// Compile with range dependencies enabled
// and logging the comparison to comparo.
// =============================================================================


// RUN: cd %t && %swiftc_driver -enable-batch-mode -j2 -incremental -enable-source-range-dependencies -driver-compare-incremental-schemes-path=./comparo -driver-show-incremental -driver-show-job-lifecycle ./main.swift ./fileA.swift ./fileB.swift -module-name main -output-file-map %t/output.json    >& %t/output2

// RUN: tail -1 %t/comparo | %FileCheck -match-full-lines -check-prefix=CHECK-COMPARO-1 %s
// CHECK-COMPARO-1: *** Incremental build disabled because different arguments passed to compiler, cannot compare ***

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-TURN-ON %s < %t/output2

// CHECK-TURN-ON-DAG: Incremental compilation has been disabled, because different arguments were passed to the compiler.
// CHECK-TURN-ON-DAG: Batchable: {compile: main.o <= main.swift}
// CHECK-TURN-ON-DAG: Batchable: {compile: fileA.o <= fileA.swift}
// CHECK-TURN-ON-DAG: Batchable: {compile: fileB.o <= fileB.swift}


// RUN: cmp main.swift main.compiledsource
// RUN: cmp fileA.swift fileA.compiledsource
// RUN: cmp fileB.swift fileB.compiledsource

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-MAIN-1 %s <%t/main.swiftranges

// CHECK-MAIN-1: ### Swift source ranges file v0 ###
// CHECK-MAIN-1-NEXT: ---
// CHECK-MAIN-1-NEXT: noninlinableFunctionBodies:
// CHECK-MAIN-1-NEXT:   - { start: { line: 5, column: 32 }, end: { line: 5, column: 39 } }
// CHECK-MAIN-1-NEXT: ...

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-FILEA-1 %s <%t/fileA.swiftranges

// CHECK-FILEA-1: ### Swift source ranges file v0 ###
// CHECK-FILEA-1: ---
// CHECK-FILEA-1: noninlinableFunctionBodies:
// CHECK-FILEA-1:   - { start: { line: 1, column: 17 }, end: { line: 4, column: 2 } }
// CHECK-FILEA-1:   - { start: { line: 5, column: 32 }, end: { line: 7, column: 2 } }
// CHECK-FILEA-1: ...


// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-FILEB-1 %s <%t/fileB.swiftranges

// CHECK-FILEB-1: ### Swift source ranges file v0 ###
// CHECK-FILEB-1: ---
// CHECK-FILEB-1: noninlinableFunctionBodies:
// CHECK-FILEB-1:   - { start: { line: 2, column: 13 }, end: { line: 2, column: 16 } }
// CHECK-FILEB-1:   - { start: { line: 5, column: 13 }, end: { line: 5, column: 15 } }
// CHECK-FILEB-1: ...


// =============================================================================
// Steady-state: Now, do it again with no changes
// =============================================================================

// RUN: cd %t && %swiftc_driver -enable-batch-mode -j2 -incremental -enable-source-range-dependencies -driver-compare-incremental-schemes-path=./comparo -driver-show-incremental -driver-show-job-lifecycle ./main.swift ./fileA.swift ./fileB.swift -module-name main -output-file-map %t/output.json  >& %t/output3

// RUN: tail -1 %t/comparo | %FileCheck -match-full-lines -check-prefix=CHECK-COMPARO-2 %s
// CHECK-COMPARO-2: *** Range benefit: 0 compilations, 0 stages, without ranges: 0, with ranges: 0, used ranges, total: 3 ***

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-NO-CHANGES %s < %t/output3
// CHECK-NO-CHANGES: (tentatively) Skipping <With ranges> file is up-to-date and output exists: {compile: main.o <= main.swift}
// CHECK-NO-CHANGES: (tentatively) Skipping <With ranges> file is up-to-date and output exists: {compile: fileA.o <= fileA.swift}
// CHECK-NO-CHANGES: (tentatively) Skipping <With ranges> file is up-to-date and output exists: {compile: fileB.o <= fileB.swift}
// CHECK-NO-CHANGES: Hypothetically: (tentatively) Skipping <Without ranges> file is up-to-date and output exists: {compile: main.o <= main.swift}
// CHECK-NO-CHANGES: Hypothetically: (tentatively) Skipping <Without ranges> file is up-to-date and output exists: {compile: fileA.o <= fileA.swift}
// CHECK-NO-CHANGES: Hypothetically: (tentatively) Skipping <Without ranges> file is up-to-date and output exists: {compile: fileB.o <= fileB.swift}
// CHECK-NO-CHANGES: Skipping <With ranges> : {compile: main.o <= main.swift}
// CHECK-NO-CHANGES: Skipping <With ranges> : {compile: fileA.o <= fileA.swift}
// CHECK-NO-CHANGES: Skipping <With ranges> : {compile: fileB.o <= fileB.swift}

// RUN: %FileCheck -check-prefix=CHECK-NO-MAIN %s < %t/output3
// RUN: %FileCheck -check-prefix=CHECK-NO-FILEA %s < %t/output3
// RUN: %FileCheck -check-prefix=CHECK-NO-FILEB %s < %t/output3
// CHECK-NO-MAIN-NOT: Added to TaskQueue: {{.*}} main.cpp
// CHECK-NO-FILEA-NOT: Added to TaskQueue: {{.*}} fileA.cpp
// CHECK-NO-FILEB-NOT: Added to TaskQueue: {{.*}} fileB.cpp



// Recheck supplementaries:
// RUN: cmp main.swift main.compiledsource
// RUN: cmp fileA.swift fileA.compiledsource
// RUN: cmp fileB.swift fileB.compiledsource
// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-MAIN-1 %s <%t/main.swiftranges
// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-FILEA-1 %s <%t/fileA.swiftranges
// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-FILEB-1 %s <%t/fileB.swiftranges

// =============================================================================
// Steady-state: Now, do it again with no changes, but touching files
// =============================================================================

// RUN: touch %t/*.swift
// RUN: cd %t && %swiftc_driver -enable-batch-mode -j2 -incremental -enable-source-range-dependencies -driver-compare-incremental-schemes-path=./comparo -driver-show-incremental -driver-show-job-lifecycle ./main.swift ./fileA.swift ./fileB.swift -module-name main -output-file-map %t/output.json  >& %t/output4


// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-COMPARO-3 %s < %t/comparo
// CHECK-COMPARO-3: *** Range benefit: 3 compilations, 1 stages, without ranges: 3, with ranges: 0, used ranges, total: 3 ***

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-TOUCHING-FILES %s < %t/output4
// CHECK-TOUCHING-FILES: (tentatively) Queuing <With ranges> (initial): {compile: main.o <= main.swift}
// CHECK-TOUCHING-FILES: Skipping <With ranges> Did not change at all: {compile: main.o <= main.swift}
// CHECK-TOUCHING-FILES: (tentatively) Queuing <With ranges> (initial): {compile: fileA.o <= fileA.swift}
// CHECK-TOUCHING-FILES: Skipping <With ranges> Did not change at all: {compile: fileA.o <= fileA.swift}
// CHECK-TOUCHING-FILES: (tentatively) Queuing <With ranges> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-TOUCHING-FILES: Skipping <With ranges> Did not change at all: {compile: fileB.o <= fileB.swift}
// CHECK-TOUCHING-FILES: Hypothetically: (tentatively) Queuing <Without ranges> (initial): {compile: main.o <= main.swift}
// CHECK-TOUCHING-FILES: Hypothetically: (tentatively) Queuing <Without ranges> (initial): {compile: fileA.o <= fileA.swift}
// CHECK-TOUCHING-FILES: Hypothetically: (tentatively) Queuing <Without ranges> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-TOUCHING-FILES: Skipping <With ranges> : {compile: main.o <= main.swift}
// CHECK-TOUCHING-FILES: Skipping <With ranges> : {compile: fileA.o <= fileA.swift}
// CHECK-TOUCHING-FILES: Skipping <With ranges> : {compile: fileB.o <= fileB.swift}

// RUN: %FileCheck -check-prefix=CHECK-NO-MAIN %s < %t/output4
// RUN: %FileCheck -check-prefix=CHECK-NO-FILEA %s < %t/output4
// RUN: %FileCheck -check-prefix=CHECK-NO-FILEB %s < %t/output4


// Recheck supplementaries:
// RUN: cmp main.swift main.compiledsource
// RUN: cmp fileA.swift fileA.compiledsource
// RUN: cmp fileB.swift fileB.compiledsource
// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-MAIN-1 %s <%t/main.swiftranges
// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-FILEA-1 %s <%t/fileA.swiftranges
// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-FILEB-1 %s <%t/fileB.swiftranges


// =============================================================================
// Make an internal change, should not recompile dependents at all
// =============================================================================

// RUN: cp %t/fileB{1-internal-change,}.swift
// RUN: cd %t && %swiftc_driver -driver-compare-incremental-schemes -enable-source-range-dependencies -output-file-map %t/output.json -incremental -enable-batch-mode ./main.swift ./fileA.swift ./fileB.swift -module-name main -j2 -driver-show-job-lifecycle -driver-show-incremental >& %t/output5

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-INTERNAL-CHANGE  %s < %t/output5
// CHECK-INTERNAL-CHANGE: Skipping <With ranges> : {compile: main.o <= main.swift}
// CHECK-INTERNAL-CHANGE: Skipping <With ranges> : {compile: fileA.o <= fileA.swift}
// CHECK-INTERNAL-CHANGE: *** Range benefit: 0 compilations, 0 stages, without ranges: 1, with ranges: 1, used ranges, total: 3 ***

// RUN: %FileCheck -check-prefix=CHECK-NO-MAIN %s < %t/output5
// RUN: %FileCheck -check-prefix=CHECK-NO-FILEA %s < %t/output5

// =============================================================================
// Make an external change, should  recompile dependents right away with ranges
// =============================================================================

// RUN: cp %t/fileB{2-external-change,}.swift
// RUN: cd %t && %swiftc_driver -driver-compare-incremental-schemes -enable-source-range-dependencies -output-file-map %t/output.json -incremental -enable-batch-mode ./main.swift ./fileA.swift ./fileB.swift -module-name main -j2 -driver-show-job-lifecycle -driver-show-incremental >& %t/output6


// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-EXERNAL-CHANGE  %s < %t/output6
// CHECK-EXERNAL-CHANGE: Queuing <With ranges> changed at [4:18--4:18): {compile: fileB.o <= fileB.swift}
// CHECK-EXERNAL-CHANGE-NEXT:   - Will immediately schedule dependents of {compile: fileB.o <= fileB.swift} because changed outside a function body at: [4:18--4:18)
// CHECK-EXERNAL-CHANGE: Queuing <With ranges> because of the initial set: {compile: fileA.o <= fileA.swift}
// CHECK-EXERNAL-CHANGE-NEXT:   fileB.swift provides type 'main.Struct1InB'
// CHECK-EXERNAL-CHANGE: Skipping <With ranges> : {compile: main.o <= main.swift}
// CHECK-EXERNAL-CHANGE: *** Range benefit: 0 compilations, 1 stages, without ranges: 2, with ranges: 2, used ranges, total: 3 ***
