// Copy in the inputs.
// The lack of a build record or swiftdeps files should disable incremental compilation

// Ensure that the extra outputs are not generated when they should not be:
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s
// RUN: ls %t | %FileCheck -check-prefix=CHECK-NO-RANGE-OUTPUTS %s
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource
// CHECK-NO-RANGE-OUTPUTS: .swiftdeps
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource


// Now, do it again with range dependencies enabled:

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-DAG: <unknown>:0: warning: unable to load dependencies file "./main.swiftdeps", disabling incremental mode
// CHECK-FIRST-DAG: Incremental compilation could not read build record.
// CHECK-FIRST-DAG: Incremental compilation has been disabled due to malformed swift dependencies file './main.swiftdeps'.

// RUN: cmp main.swift main.compiledsource
// RUN: cmp fileA.swift fileA.compiledsource
// RUN: cmp fileB.swift fileB.compiledsource

// RUN: %FileCheck -check-prefix=CHECK-MAIN1-RANGES %s <%t/main.swiftranges

// CHECK-MAIN1-RANGES: ### Swift source ranges file v0 ###
// CHECK-MAIN1-RANGES: ---
// CHECK-MAIN1-RANGES: unparsedRangesByNonPrimary:
// CHECK-MAIN1-RANGES:   ./fileB.swift:
// CHECK-MAIN1-RANGES:     - { start: { line: 1, column: 18 }, end: { line: 6, column: 2 } }
// CHECK-MAIN1-RANGES:     - { start: { line: 7, column: 19 }, end: { line: 10, column: 2 } }
// CHECK-MAIN1-RANGES: noninlinableFunctionBodies: []
// CHECK-MAIN1-RANGES: ...

// RUN: %FileCheck -check-prefix=CHECK-FILEA1-RANGES %s <%t/fileA.swiftranges

// CHECK-FILEA1-RANGES: ### Swift source ranges file v0 ###
// CHECK-FILEA1-RANGES: ---
// CHECK-FILEA1-RANGES: unparsedRangesByNonPrimary:
// CHECK-FILEA1-RANGES:   ./fileB.swift:
// CHECK-FILEA1-RANGES:     - { start: { line: 7, column: 19 }, end: { line: 10, column: 2 } }
// CHECK-FILEA1-RANGES: noninlinableFunctionBodies:
// CHECK-FILEA1-RANGES:   - { start: { line: 1, column: 17 }, end: { line: 4, column: 2 } }
// CHECK-FILEA1-RANGES:   - { start: { line: 5, column: 17 }, end: { line: 7, column: 2 } }
// CHECK-FILEA1-RANGES:   - { start: { line: 8, column: 17 }, end: { line: 8, column: 19 } }
// CHECK-FILEA1-RANGES: ...


// RUN: %FileCheck -check-prefix=CHECK-FILEB1-RANGES %s <%t/fileB.swiftranges

// CHECK-FILEB1-RANGES: ### Swift source ranges file v0 ###
// CHECK-FILEB1-RANGES: ---
// CHECK-FILEB1-RANGES: unparsedRangesByNonPrimary: {}
// CHECK-FILEB1-RANGES: noninlinableFunctionBodies: []
// CHECK-FILEB1-RANGES: ...



// Add an attribute to: a structure that no other file uses

// RUN: cp %t/fileB2.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >%t/output 2>&1
// RUN: %FileCheck -check-prefix=CHECK-ONLY-B %s <%t/output

// CHECK-ONLY-B-NOT: Queuing{{.*}}<= main.swift
// CHECK-ONLY-B-NOT: Queuing{{.*}}<= fileA.swift

// RUN: %FileCheck -check-prefix=CHECK-B %s <%t/output

// CHECK-B: Queuing{{.*}}<= fileB.swift{{.*}}Ranges

// RUN: rm %t/output


// Add an attribute to: a structure that one other file uses

// RUN: cp %t/fileB3.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental -driver-dump-compiled-source-diffs >%t/output  2>&1

// RUN: %FileCheck -check-prefix=CHECK-ONLY-AB %s <%t/output

// CHECK-ONLY-AB-NOT: Queuing{{.*}}<= main.swift

// RUN: %FileCheck -check-prefix=CHECK-AB %s <%t/output

// CHECK-AB-DAG: Queuing{{.*}}<= fileB.swift{{.*}}Ranges
// CHECK-AB-DAG: Queuing{{.*}}<= fileA.swift{{.*}}Ranges

// RUN: %FileCheck -check-prefix=CHECK-DIFFS1 %s <%t/output

// CHECK-DIFFS1: *** all changed ranges in './fileA.swift' ***
// CHECK-DIFFS1-NEXT: {{^$}}
// CHECK-DIFFS1-NEXT: *** nonlocal changed ranges in './fileA.swift' ***
// CHECK-DIFFS1-NEXT: {{^$}}
// CHECK-DIFFS1-NEXT: {{^$}}
// CHECK-DIFFS1-NEXT: *** all changed ranges in './fileB.swift' ***
// CHECK-DIFFS1-NEXT: [6:1--6:1)
// CHECK-DIFFS1-NEXT: {{^$}}
// CHECK-DIFFS1-NEXT: *** nonlocal changed ranges in './fileB.swift' ***
// CHECK-DIFFS1-NEXT: [6:1--6:1)
// CHECK-DIFFS1-NEXT: {{^$}}
// CHECK-DIFFS1-NEXT: {{^$}}
// CHECK-DIFFS1-NEXT: *** all changed ranges in './main.swift' ***
// CHECK-DIFFS1-NEXT: {{^$}}
// CHECK-DIFFS1-NEXT: *** nonlocal changed ranges in './main.swift' ***
// CHECK-DIFFS1-NEXT: {{^$}}


// RUN: gazorp

// What if the user adds a close brace and new type in the middle?

// RUN: cp %t/fileB4.swift %t/fileB.swift
// RUN: cd %t && %swiftc_driver -enable-source-range-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental >%t/output 2>&1
// RUN: %FileCheck -check-prefix=CHECK-ALL-REBUILT %s <%t/output

// CHECK-NEW-TOP-NOT: Queuing{{.*}}<= main.swift

// RUN: %FileCheck -check-prefix=CHECK-ALL-REBUILT %s <%t/output

// CHECK-ALL-REBUILT: Queuing{{.*}}<= fileB.swift{{.*}}Ranges
// CHECK-ALL-REBUILT: Queuing{{.*}}<= main.swift{{.*}}Ranges
// CHECK-ALL-REBUILT: Queuing because of dependencies discovered later: {compile: fileA.o <= fileA.swift} <Ranges>
