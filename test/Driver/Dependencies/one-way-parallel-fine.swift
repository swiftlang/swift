// Windows doesn't support parallel execution yet
// XFAIL: OS=windows-msvc
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ *}}: "began"
// CHECK-FIRST-DAG: "name"{{ *}}: "compile"
// CHECK-FIRST-DAG: "{{(.\\/)?}}main.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ *}}: "finished"
// CHECK-FIRST-DAG: "name"{{ *}}: "compile"
// CHECK-FIRST-DAG: "output"{{ *}}: "Handled main.swift\n"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ *}}: "began"
// CHECK-FIRST-DAG: "name"{{ *}}: "compile"
// CHECK-FIRST-DAG: "{{(.\\/)?}}other.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ *}}: "finished"
// CHECK-FIRST-DAG: "name"{{ *}}: "compile"
// CHECK-FIRST-DAG: "output"{{ *}}: "Handled other.swift\n"
// CHECK-FIRST: {{^}$}}

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j2 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND-DAG: "kind"{{ *}}: "began"
// CHECK-SECOND-DAG: "name"{{ *}}: "compile"
// CHECK-SECOND-DAG: "{{(.\\/)?}}other.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND-DAG: "kind"{{ *}}: "began"
// CHECK-SECOND-DAG: "name"{{ *}}: "compile"
// CHECK-SECOND-DAG: "{{(.\\/)?}}main.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND-DAG: "kind"{{ *}}: "finished"
// CHECK-SECOND-DAG: "name"{{ *}}: "compile"
// CHECK-SECOND-DAG: "output"{{ *}}: "Handled {{other.swift|main.swift}}\n"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND-DAG: "kind"{{ *}}: "finished"
// CHECK-SECOND-DAG: "name"{{ *}}: "compile"
// CHECK-SECOND-DAG: "output"{{ *}}: "Handled {{main.swift|other.swift}}\n"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND-NOT: "skipped"
