// Copy in the inputs.
// The lack of a build record or swiftdeps files should disable incremental compilation

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-lifecycle/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

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
// CHECK-MAIN1-RANGES:     - { start: { line: 1, column: 17 }, end: { line: 6, column: 2 } }
// CHECK-MAIN1-RANGES:     - { start: { line: 7, column: 16 }, end: { line: 10, column: 2 } }
// CHECK-MAIN1-RANGES: noninlinableFunctionBodies: []
// CHECK-MAIN1-RANGES: ...

// RUN: %FileCheck -check-prefix=CHECK-FILEA1-RANGES %s <%t/fileA.swiftranges

// CHECK-FILEA1-RANGES: ### Swift source ranges file v0 ###
// CHECK-FILEA1-RANGES: ---
// CHECK-FILEA1-RANGES: unparsedRangesByNonPrimary:
// CHECK-FILEA1-RANGES:   ./fileB.swift:
// CHECK-FILEA1-RANGES:     - { start: { line: 7, column: 16 }, end: { line: 10, column: 2 } }
// CHECK-FILEA1-RANGES: noninlinableFunctionBodies:
// CHECK-FILEA1-RANGES:   - { start: { line: 1, column: 14 }, end: { line: 4, column: 2 } }
// CHECK-FILEA1-RANGES:   - { start: { line: 5, column: 13 }, end: { line: 7, column: 2 } }
// CHECK-FILEA1-RANGES:   - { start: { line: 8, column: 12 }, end: { line: 8, column: 14 } }
// CHECK-FILEA1-RANGES: ...

// RUN: %FileCheck -check-prefix=CHECK-FILEB1-RANGES %s <%t/fileB.swiftranges

// CHECK-FILEB1-RANGES: ### Swift source ranges file v0 ###
// CHECK-FILEB1-RANGES: ---
// CHECK-FILEB1-RANGES: unparsedRangesByNonPrimary: {}
// CHECK-FILEB1-RANGES: noninlinableFunctionBodies: []
// CHECK-FILEB1-RANGES: ...



// RUN: cd %t && %swiftc_driver -c -output-file-map %t/output.json -incremental ./main.swift ./fileA.swift ./fileB.swift -module-name main -j1 -driver-show-incremental 2>&1 | tee /tmp/out | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND




// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-THIRD %s

// CHECK-THIRD-DAG: Handled other.swift
// CHECK-THIRD-DAG: Handled main.swift
// CHECK-THIRD-DAG: Handled yet-another.swift

// RUN: touch -t 201401240007 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./other.swift ./main.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-THIRD %s

// RUN: touch -t 201401240008 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./yet-another.swift ./other.swift ./main.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-THIRD %s

// RUN: touch -t 201401240009 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./other.swift ./yet-another.swift ./main.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-THIRD %s
