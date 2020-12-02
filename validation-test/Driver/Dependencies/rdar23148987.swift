// RUN: %empty-directory(%t)

// RUN: cp %s %t/main.swift
// RUN: cp %S/Inputs/rdar23148987/helper-1.swift %t/helper.swift
// RUN: touch -t 201401240005 %t/*.swift

// RUN: cd %t && %target-build-swift -c -incremental -output-file-map %S/Inputs/rdar23148987/output.json -parse-as-library ./main.swift ./helper.swift -parseable-output -j1 -module-name main 2>&1 | %FileCheck -check-prefix=CHECK-1 %s

// CHECK-1-NOT: warning
// CHECK-1: {{^{$}}
// CHECK-1-DAG: "kind"{{ ?}}: "began"
// CHECK-1-DAG: "name"{{ ?}}: "compile"
// CHECK-1-DAG: "{{(\.\\\/)?}}main.swift"
// CHECK-1: {{^}$}}

// CHECK-1: {{^{$}}
// CHECK-1-DAG: "kind"{{ ?}}: "began"
// CHECK-1-DAG: "name"{{ ?}}: "compile"
// CHECK-1-DAG: "{{(\.\\\/)?}}helper.swift"
// CHECK-1: {{^}$}}

// RUN: ls %t/ | %FileCheck -check-prefix=CHECK-LS %s

// CHECK-LS-DAG: main.o
// CHECK-LS-DAG: helper.o

// RUN: cd %t && %target-build-swift -c -incremental -output-file-map %S/Inputs/rdar23148987/output.json -parse-as-library ./main.swift ./helper.swift -parseable-output -j1 -module-name main 2>&1 | %FileCheck -check-prefix=CHECK-1-SKIPPED %s

// CHECK-1-SKIPPED-NOT: warning
// CHECK-1-SKIPPED: {{^{$}}
// CHECK-1-SKIPPED-DAG: "kind"{{ ?}}: "skipped"
// CHECK-1-SKIPPED-DAG: "name"{{ ?}}: "compile"
// CHECK-1-SKIPPED-DAG: "{{(\.\\\/)?}}main.swift"
// CHECK-1-SKIPPED: {{^}$}}

// CHECK-1-SKIPPED: {{^{$}}
// CHECK-1-SKIPPED-DAG: "kind"{{ ?}}: "skipped"
// CHECK-1-SKIPPED-DAG: "name"{{ ?}}: "compile"
// CHECK-1-SKIPPED-DAG: "{{(\.\\\/)?}}helper.swift"
// CHECK-1-SKIPPED: {{^}$}}

// RUN: cp %S/Inputs/rdar23148987/helper-2.swift %t/helper.swift
// RUN: touch -t 201401240006 %t/helper.swift
// RUN: cd %t && %target-build-swift -c -incremental -output-file-map %S/Inputs/rdar23148987/output.json -parse-as-library ./main.swift ./helper.swift -parseable-output -j1 -module-name main 2>&1 -driver-show-incremental -driver-show-job-lifecycle | %FileCheck -check-prefix=CHECK-2 %s

// CHECK-2-NOT: warning
// CHECK-2: {{^{$}}
// CHECK-2-DAG: "kind"{{ ?}}: "began"
// CHECK-2-DAG: "name"{{ ?}}: "compile"
// CHECK-2-DAG: "{{(\.\\\/)?}}helper.swift"
// CHECK-2: {{^}$}}

// CHECK-2: {{^{$}}
// CHECK-2-DAG: "kind"{{ ?}}: "began"
// CHECK-2-DAG: "name"{{ ?}}: "compile"
// CHECK-2-DAG: "{{(\.\\\/)?}}main.swift"
// CHECK-2: {{^}$}}

func test(obj: Test) {
  obj.foo()
}
