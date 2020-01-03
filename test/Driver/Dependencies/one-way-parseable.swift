// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "began"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: ".\/main.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "finished"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: "output": "Handled main.swift{{(\\r)?}}\n"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "began"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: ".\/other.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind": "finished"
// CHECK-FIRST: "name": "compile"
// CHECK-FIRST: "output": "Handled other.swift{{(\\r)?}}\n"
// CHECK-FIRST: {{^}$}}

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND: "kind": "skipped"
// CHECK-SECOND: "name": "compile"
// CHECK-SECOND: ".\/main.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND: "kind": "skipped"
// CHECK-SECOND: "name": "compile"
// CHECK-SECOND: ".\/other.swift"
// CHECK-SECOND: {{^}$}}

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-THIRD %s

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD: "kind": "began"
// CHECK-THIRD: "name": "compile"
// CHECK-THIRD: ".\/main.swift"
// CHECK-THIRD: {{^}$}}

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD: "kind": "finished"
// CHECK-THIRD: "name": "compile"
// CHECK-THIRD: "output": "Handled main.swift{{(\\r)?}}\n"
// CHECK-THIRD: {{^}$}}

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD: "kind": "began"
// CHECK-THIRD: "name": "compile"
// CHECK-THIRD: ".\/other.swift"
// CHECK-THIRD: {{^}$}}

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD: "kind": "finished"
// CHECK-THIRD: "name": "compile"
// CHECK-THIRD: "output": "Handled other.swift{{(\\r)?}}\n"
// CHECK-THIRD: {{^}$}}

// RUN: touch -t 201401240006 %t/main.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FOURTH %s

// CHECK-FOURTH: {{^{$}}
// CHECK-FOURTH: "kind": "began"
// CHECK-FOURTH: "name": "compile"
// CHECK-FOURTH: ".\/main.swift"
// CHECK-FOURTH: {{^}$}}

// CHECK-FOURTH: {{^{$}}
// CHECK-FOURTH: "kind": "finished"
// CHECK-FOURTH: "name": "compile"
// CHECK-FOURTH: "output": "Handled main.swift{{(\\r)?}}\n"
// CHECK-FOURTH: {{^}$}}

// CHECK-FOURTH: {{^{$}}
// CHECK-FOURTH: "kind": "skipped"
// CHECK-FOURTH: "name": "compile"
// CHECK-FOURTH: ".\/other.swift"
// CHECK-FOURTH: {{^}$}}
