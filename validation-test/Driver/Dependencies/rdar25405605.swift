// RUN: %empty-directory(%t)

// RUN: cp %s %t/main.swift
// RUN: cp %S/Inputs/rdar25405605/helper-1.swift %t/helper.swift
// RUN: touch -t 201401240005 %t/*.swift

// RUN: cd %t && %target-build-swift -c -incremental -output-file-map %S/Inputs/rdar25405605/output.json -parse-as-library ./main.swift ./helper.swift -parseable-output -j1 -module-name main 2>&1 | %FileCheck -check-prefix=CHECK-1 %s

// CHECK-1-NOT: warning
// CHECK-1: {{^{$}}
// CHECK-1: "kind": "began"
// CHECK-1: "name": "compile"
// CHECK-1: ".\/main.swift"
// CHECK-1: {{^}$}}

// CHECK-1: {{^{$}}
// CHECK-1: "kind": "began"
// CHECK-1: "name": "compile"
// CHECK-1: ".\/helper.swift"
// CHECK-1: {{^}$}}

// RUN: ls %t/ | %FileCheck -check-prefix=CHECK-LS %s

// CHECK-LS-DAG: main.o
// CHECK-LS-DAG: helper.o

// RUN: cd %t && %target-build-swift -c -incremental -output-file-map %S/Inputs/rdar25405605/output.json -parse-as-library ./main.swift ./helper.swift -parseable-output -j1 -module-name main 2>&1 | %FileCheck -check-prefix=CHECK-1-SKIPPED %s

// CHECK-1-SKIPPED-NOT: warning
// CHECK-1-SKIPPED: {{^{$}}
// CHECK-1-SKIPPED: "kind": "skipped"
// CHECK-1-SKIPPED: "name": "compile"
// CHECK-1-SKIPPED: ".\/main.swift"
// CHECK-1-SKIPPED: {{^}$}}

// CHECK-1-SKIPPED: {{^{$}}
// CHECK-1-SKIPPED: "kind": "skipped"
// CHECK-1-SKIPPED: "name": "compile"
// CHECK-1-SKIPPED: ".\/helper.swift"
// CHECK-1-SKIPPED: {{^}$}}

// RUN: cp %S/Inputs/rdar25405605/helper-2.swift %t/helper.swift
// RUN: touch -t 201401240006 %t/helper.swift
// RUN: cd %t && not %target-build-swift -c -incremental -output-file-map %S/Inputs/rdar25405605/output.json -parse-as-library ./main.swift ./helper.swift -parseable-output -j1 -module-name main 2>&1 | %FileCheck -check-prefix=CHECK-2 %s

// CHECK-2-NOT: warning
// CHECK-2: {{^{$}}
// CHECK-2: "kind": "began"
// CHECK-2: "name": "compile"
// CHECK-2: ".\/helper.swift"
// CHECK-2: {{^}$}}

// CHECK-2: {{^{$}}
// CHECK-2: "kind": "skipped"
// CHECK-2: "name": "compile"
// CHECK-2: ".\/main.swift"
// CHECK-2: {{^}$}}

// RUN: cp %S/Inputs/rdar25405605/helper-3.swift %t/helper.swift
// RUN: touch -t 201401240007 %t/helper.swift
// Driver now schedules jobs in the order of the inputs, so since this test wants
// helper first, pass helper.swift before main.swift
// RUN: cd %t && not %target-build-swift -c -incremental -output-file-map %S/Inputs/rdar25405605/output.json -parse-as-library ./helper.swift ./main.swift -parseable-output -j1 -module-name main 2>&1 | %FileCheck -check-prefix=CHECK-3 %s

// CHECK-3-NOT: warning
// CHECK-3: {{^{$}}
// CHECK-3: "kind": "began"
// CHECK-3: "name": "compile"
// CHECK-3: ".\/helper.swift"
// CHECK-3: {{^}$}}

// CHECK-3: {{^{$}}
// CHECK-3: "kind": "began"
// CHECK-3: "name": "compile"
// CHECK-3: ".\/main.swift"
// CHECK-3: {{^}$}}

func foo(_ value: Foo) -> Bool {
    switch value {
    case .one: return true
    case .two: return false
    }
}
