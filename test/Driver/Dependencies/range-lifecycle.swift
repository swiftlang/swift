// Tests how range-based incremental compilation copes wtih various changes.

// Copy in the inputs.
// The lack of a build record or swiftdeps files should disable incremental compilation



// Ensure that the extra outputs are not generated when they should not be:
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >& %t/output1
// RUN: %FileCheck -check-prefix=CHECK-NO-BUILD-REC %s < %t/output1
// RUN: ls %t | %FileCheck -check-prefix=CHECK-NO-RANGE-OUTPUTS %s
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource
// CHECK-NO-RANGE-OUTPUTS: .swiftdeps
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource



// Now, do it again with range dependencies enabled:

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >& %t/output2
// RUN: %FileCheck -check-prefix=CHECK-NO-BUILD-REC %s < %t/output2

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
// CHECK-MAIN-RANGES-1-NEXT:     - { start: { line: 1, column: 19 }, end: { line: 6, column: 2 } }
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
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >& %t/output3
// RUN: %FileCheck -check-prefix=CHECK-FILEB-ONLY %s < %t/output3

// CHECK-FILEB-ONLY-NOT: Queuing{{.*}}<= main.swift
// CHECK-FILEB-ONLY-NOT: Queuing{{.*}}<= fileA.swift


// RUN: %FileCheck -check-prefix=CHECK-FILEB-AND-SELECTING-RANGES %s < %t/output3

// CHECK-FILEB-AND-SELECTING-RANGES: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-FILEB-AND-SELECTING-RANGES: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}
// CHECK-FILEB-AND-SELECTING-RANGES: Using ranges

// Add an attribute to: a structure that one other file uses

// RUN: cp %t/fileB3.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental -driver-dump-compiled-source-diffs >& %t/output4

// RUN: %FileCheck -check-prefix=CHECK-FILEA-AND-FILEB-ONLY %s < %t/output4

// CHECK-FILEA-AND-FILEB-ONLY-NOT: Queuing{{.*}}<= main.swift

// RUN: %FileCheck -check-prefix=CHECK-DIFFS-NONE-1 %s < %t/output4
// CHECK-DIFFS-NONE-1-DAG: *** no changed ranges in 'main.swift' (w.r.t previously- or about-to-be-compiled) ***
// CHECK-DIFFS-NONE-1-DAG: *** no changed ranges in 'fileA.swift' (w.r.t previously- or about-to-be-compiled) ***

// RUN: %FileCheck -check-prefix=CHECK-DIFFS-FILEB-1 %s < %t/output4
// CHECK-DIFFS-FILEB-1: *** all changed ranges in 'fileB.swift' (w.r.t previously-compiled) ***
// CHECK-DIFFS-FILEB-1-NEXT: - [4:18--4:19)
// CHECK-DIFFS-FILEB-1-NEXT: *** all changed ranges in 'fileB.swift' (w.r.t to-be-compiled) ***
// CHECK-DIFFS-FILEB-1-NEXT: - [4:18--4:28)
// CHECK-DIFFS-FILEB-1-NEXT: *** nonlocal changed ranges in 'fileB.swift' (w.r.t previously-compiled) ***
// CHECK-DIFFS-FILEB-1-NEXT: - [4:18--4:19)

// RUN: %FileCheck -check-prefix=CHECK-FILEA-FILEB-SELECTING-RANGES %s < %t/output4

// CHECK-FILEA-FILEB-SELECTING-RANGES: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-FILEA-FILEB-SELECTING-RANGES: Queuing <Ranges> (changed: fileB.swift:[4:18--4:19)): {compile: fileA.o <= fileA.swift}
// CHECK-FILEA-FILEB-SELECTING-RANGES: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}
// CHECK-FILEA-FILEB-SELECTING-RANGES: Using ranges


// What if the user adds a close brace and new type in the middle?

// RUN: cp %t/fileB4.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >& %t/output5

// RUN: %FileCheck -check-prefix=CHECK-INITIALLY-ABSENT-MAIN %s < %t/output5

// CHECK-INITIALLY-ABSENT-MAIN-NOT: Queueing{{.*}}<= main.swift

// RUN: %FileCheck -check-prefix=CHECK-A-B-RANGES-THEN-MAIN %s < %t/output5

// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Dependencies> (initial): {compile: fileB.o <= fileB.swift}
// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Ranges> (changed: fileB.swift:[5:3--5:26)): {compile: fileA.o <= fileA.swift}
// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Ranges> (this file changed): {compile: fileB.o <= fileB.swift}

// CHECK-A-B-RANGES-THEN-MAIN: Using ranges

// CHECK-A-B-RANGES-THEN-MAIN: After completion of {compile: fileB.o <= fileB.swift}:
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Dependencies would now schedule: {compile: main.o <= main.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Dependencies would now schedule: {compile: fileA.o <= fileA.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Source ranges will now schedule: {compile: main.o <= main.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: - Source ranges will now schedule: {compile: fileA.o <= fileA.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT: For an additional 2 (deps) vs 2 (ranges)
// CHECK-A-B-RANGES-THEN-MAIN: Queuing <Ranges> because of dependencies discovered later: {compile: main.o <= main.swift}
// CHECK-A-B-RANGES-THEN-MAIN-NEXT:   fileB.swift provides top-level name 'watchMe'

// What happens when a new file is added?

// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift ./fileC.swift -module-name main -j1 -driver-show-incremental >& %t/output6

// RUN: cmp fileB.swift fileB.compiledsource

// RUN: %FileCheck -check-prefix=CHECK-FILEC-RANGES-1 %s <%t/fileC.swiftranges
// CHECK-FILEC-RANGES-1: ### Swift source ranges file v0 ###
// CHECK-FILEC-RANGES-1-NEXT: ---
// CHECK-FILEC-RANGES-1-NEXT: unparsedRangesByNonPrimary: {}
// CHECK-FILEC-RANGES-1-NEXT: noninlinableFunctionBodies:
// CHECK-FILEC-RANGES-1-NEXT:   - { start: { line: 3, column: 19 }, end: { line: 3, column: 21 } }
// CHECK-FILEC-RANGES-1-NEXT:   - { start: { line: 5, column: 26 }, end: { line: 5, column: 28 } }
// CHECK-FILEC-RANGES-1-NEXT: ...

// RUN: %FileCheck -check-prefix=CHECK-ADD-NEW-FILE %s < %t/output6

// CHECK-ADD-NEW-FILE-DAG: unable to load swift ranges file "./fileC.swiftranges", No such file or directory
// CHECK-ADD-NEW-FILE-DAG: unable to determine when 'fileC.compiledsource' was last modified: No such file or directory
// CHECK-ADD-NEW-FILE-DAG: Queuing <Dependencies> (initial): {compile: fileC.o <= fileC.swift}
// CHECK-ADD-NEW-FILE-DAG: Using dependenciess: Some input lacks supplementary output needed for the source range strategy.
// CHECK-ADD-NEW-FILE-DAG: Queuing <Dependencies> because of dependencies discovered later: {compile: fileB.o <= fileB.swift}
// CHECK-ADD-NEW-FILE-DAG: Queuing <Dependencies> because of dependencies discovered later: {compile: fileA.o <= fileA.swift}
// CHECK-ADD-NEW-FILE-DAG: Queuing <Dependencies> because of dependencies discovered later: {compile: main.o <= main.swift}
