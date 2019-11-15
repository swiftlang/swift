// Copy in the inputs.
// The lack of a build record or swiftdeps files should disable incremental compilation

// Ensure that the extra outputs are not generated when they should not be:
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-NO-BUILD-REC %s
// RUN: ls %t | %FileCheck -check-prefix=CHECK-NO-RANGE-OUTPUTS %s
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource
// CHECK-NO-RANGE-OUTPUTS: .swiftdeps
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource


// Now, do it again with range dependencies enabled:

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-NO-BUILD-REC %s

// CHECK-NO-BUILD-REC-DAG: <unknown>:0: warning: unable to load dependencies file "./main.swiftdeps", disabling incremental mode
// CHECK-NO-BUILD-REC-DAG: Incremental compilation could not read build record.
// CHECK-NO-BUILD-REC-DAG: Incremental compilation has been disabled due to malformed swift dependencies file './main.swiftdeps'.

// RUN: cmp main.swift main.compiledsource
// RUN: cmp fileA.swift fileA.compiledsource
// RUN: cmp fileB.swift fileB.compiledsource

// RUN: %FileCheck -check-prefix=CHECK-MAIN-RANGES-1 %s <%t/main.swiftranges

// CHECK-MAIN-RANGES-1: ### Swift source ranges file v0 ###
// CHECK-MAIN-RANGES-1-NEXT: ---
// CHECK-MAIN-RANGES-1-NEXT: unparsedRangesByNonPrimary:
// CHECK-MAIN-RANGES-1-NEXT:   ./fileB.swift:
// CHECK-MAIN-RANGES-1-NEXT:     - { start: { line: 1, column: 18 }, end: { line: 6, column: 2 } }
// CHECK-MAIN-RANGES-1-NEXT:     - { start: { line: 7, column: 19 }, end: { line: 10, column: 2 } }
// CHECK-MAIN-RANGES-1-NEXT: noninlinableFunctionBodies:
// CHECK-MAIN-RANGES-1-NEXT:  - { start: { line: 2, column: 22 }, end: { line: 2, column: 24 } }
// CHECK-MAIN-RANGES-1-NEXT: ...

// RUN: %FileCheck -check-prefix=CHECK-FILEA-RANGES-1 %s <%t/fileA.swiftranges

// CHECK-FILEA-RANGES-1: ### Swift source ranges file v0 ###
// CHECK-FILEA-RANGES-1: ---
// CHECK-FILEA-RANGES-1: unparsedRangesByNonPrimary:
// CHECK-FILEA-RANGES-1:   ./fileB.swift:
// CHECK-FILEA-RANGES-1:     - { start: { line: 7, column: 19 }, end: { line: 10, column: 2 } }
// CHECK-FILEA-RANGES-1: noninlinableFunctionBodies:
// CHECK-FILEA-RANGES-1:   - { start: { line: 1, column: 17 }, end: { line: 4, column: 2 } }
// CHECK-FILEA-RANGES-1:   - { start: { line: 5, column: 17 }, end: { line: 7, column: 2 } }
// CHECK-FILEA-RANGES-1:   - { start: { line: 8, column: 17 }, end: { line: 8, column: 19 } }
// CHECK-FILEA-RANGES-1: ...


// RUN: %FileCheck -check-prefix=CHECK-FILEB-RANGES-1 %s <%t/fileB.swiftranges

// CHECK-FILEB-RANGES-1: ### Swift source ranges file v0 ###
// CHECK-FILEB-RANGES-1: ---
// CHECK-FILEB-RANGES-1: unparsedRangesByNonPrimary: {}
// CHECK-FILEB-RANGES-1: noninlinableFunctionBodies:
// CHECK-FILEB-RANGES-1:   - { start: { line: 5, column: 24 }, end: { line: 5, column: 26 } }
// CHECK-FILEB-RANGES-1: ...


// Add an attribute to: a structure that no other file uses

// RUN: cp %t/fileB2.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >%t/output1 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FILEB-ONLY %s <%t/output1

// CHECK-FILEB-ONLY-NOT: Queuing{{.*}}<= main.swift
// CHECK-FILEB-ONLY-NOT: Queuing{{.*}}<= fileA.swift

// RUN: %FileCheck -check-prefix=CHECK-FILEB-AND-SELECTING-RANGES %s <%t/output1

// CHECK-FILEB-AND-SELECTING-RANGES: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-FILEB-AND-SELECTING-RANGES: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}
// CHECK-FILEB-AND-SELECTING-RANGES: Selecting source ranges

// Add an attribute to: a structure that one other file uses

// RUN: cp %t/fileB3.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental -driver-dump-compiled-source-diffs >%t/output2  2>&1

// RUN: %FileCheck -check-prefix=CHECK-FILEA-AND-FILEB-ONLY %s <%t/output2

// CHECK-FILEA-AND-FILEB-ONLY-NOT: Queuing{{.*}}<= main.swift

// RUN: %FileCheck -check-prefix=CHECK-DIFFS-1 %s <%t/output2

// CHECK-DIFFS-1: *** no changed ranges in previously-compiled 'fileA.swift' ***
// CHECK-DIFFS-1: *** all changed ranges in previously-compiled 'fileB.swift' ***
// CHECK-DIFFS-1-NEXT: [4:18--4:19)
// CHECK-DIFFS-1: *** nonlocal changed ranges in previously-compiled 'fileB.swift' ***
// CHECK-DIFFS-1-NEXT: [4:18--4:19)
// CHECK-DIFFS-1: *** no changed ranges in previously-compiled 'main.swift' ***

// RUN: %FileCheck -check-prefix=CHECK-FILEA-FILEB-SELECTING-RANGES %s <%t/output2

// CHECK-FILEA-FILEB-SELECTING-RANGES: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-FILEA-FILEB-SELECTING-RANGES: Queuing <Ranges> (changed: fileB.swift:[4:18--4:19)): {compile: fileA.o <= fileA.swift}
// CHECK-FILEA-FILEB-SELECTING-RANGES: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}
// CHECK-FILEA-FILEB-SELECTING-RANGES: Selecting source ranges


// What if the user adds a close brace and new type in the middle?

// RUN: cp %t/fileB4.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >%t/output3 2>&1

// RUN: %FileCheck -check-prefix=CHECK-INITIALLY-ABSENT-MAIN %s <%t/output3

// CHECK-INITIALLY-ABSENT-MAIN-NOT: Queueing{{.*}}<= main.swift

// RUN: %FileCheck -check-prefix=CHECK-A-B-RANGES-THEN-MAIN %s <%t/output3

// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Ranges> (changed: fileB.swift:[5:3--5:26)): {compile: fileA.o <= fileA.swift}
// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}

// CHECK-A-B-RANGES-THEN-MAIN: Selecting source ranges

// CHECK-A-B-RANGES-THEN-MAIN: After completion of {compile: fileB.o <= fileB.swift}:
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Dependencies would now schedule: {compile: main.o <= main.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Dependencies would now schedule: {compile: fileA.o <= fileA.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Source ranges will now schedule: {compile: main.o <= main.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Source ranges will now schedule: {compile: fileA.o <= fileA.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: For an additional 2 (deps) vs 2 (ranges)
// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Ranges> because of dependencies discovered later: {compile: main.o <= main.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT:   fileB.swift provides top-level name 'watchMe'
