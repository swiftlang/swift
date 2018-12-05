
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "began"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: ".\/main.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "finished"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: "output": "Handled main.swift\n"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "began"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: ".\/other.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "finished"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: "output": "Handled other.swift\n"
// CHECK-FIRST: {{^}$}}

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j2 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND: "kind": "began"
// CHECK-SECOND: "name": "compile"
// CHECK-SECOND: ".\/other.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND: "kind": "began"
// CHECK-SECOND: "name": "compile"
// CHECK-SECOND: ".\/main.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND: "kind": "finished"
// CHECK-SECOND: "name": "compile"
// CHECK-SECOND: "output": "Handled {{other.swift|main.swift}}\n"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND: "kind": "finished"
// CHECK-SECOND: "name": "compile"
// CHECK-SECOND: "output": "Handled {{main.swift|other.swift}}\n"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND-NOT: "skipped"
