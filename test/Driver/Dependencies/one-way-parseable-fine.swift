// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "{{(.\\/)?}}main.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "finished"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "output"{{ ?}}: "Handled main.swift{{(\\r)?}}\n"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "{{(.\\/)?}}other.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "finished"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "output"{{ ?}}: "Handled other.swift{{(\\r)?}}\n"
// CHECK-FIRST: {{^}$}}

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND-DAG: "kind"{{ ?}}: "skipped"
// CHECK-SECOND-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-DAG: "{{(.\\/)?}}main.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND-DAG: "kind"{{ ?}}: "skipped"
// CHECK-SECOND-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-DAG: "{{(.\\/)?}}other.swift"
// CHECK-SECOND: {{^}$}}

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-THIRD %s

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD-DAG: "kind"{{ ?}}: "began"
// CHECK-THIRD-DAG: "name"{{ ?}}: "compile"
// CHECK-THIRD-DAG: "{{(.\\/)?}}main.swift"
// CHECK-THIRD: {{^}$}}

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD-DAG: "kind"{{ ?}}: "finished"
// CHECK-THIRD-DAG: "name"{{ ?}}: "compile"
// CHECK-THIRD-DAG: "output"{{ ?}}: "Handled main.swift{{(\\r)?}}\n"
// CHECK-THIRD: {{^}$}}

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD-DAG: "kind"{{ ?}}: "began"
// CHECK-THIRD-DAG: "name"{{ ?}}: "compile"
// CHECK-THIRD-DAG: "{{(.\\/)?}}other.swift"
// CHECK-THIRD: {{^}$}}

// CHECK-THIRD: {{^{$}}
// CHECK-THIRD-DAG: "kind"{{ ?}}: "finished"
// CHECK-THIRD-DAG: "name"{{ ?}}: "compile"
// CHECK-THIRD-DAG: "output"{{ ?}}: "Handled other.swift{{(\\r)?}}\n"
// CHECK-THIRD: {{^}$}}

// RUN: touch -t 201401240006 %t/main.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FOURTH %s

// CHECK-FOURTH: {{^{$}}
// CHECK-FOURTH-DAG: "kind"{{ ?}}: "began"
// CHECK-FOURTH-DAG: "name"{{ ?}}: "compile"
// CHECK-FOURTH-DAG: "{{(.\\/)?}}main.swift"
// CHECK-FOURTH: {{^}$}}

// CHECK-FOURTH: {{^{$}}
// CHECK-FOURTH-DAG: "kind"{{ ?}}: "finished"
// CHECK-FOURTH-DAG: "name"{{ ?}}: "compile"
// CHECK-FOURTH-DAG: "output"{{ ?}}: "Handled main.swift{{(\\r)?}}\n"
// CHECK-FOURTH: {{^}$}}

// CHECK-FOURTH: {{^{$}}
// CHECK-FOURTH-DAG: "kind"{{ ?}}: "skipped"
// CHECK-FOURTH-DAG: "name"{{ ?}}: "compile"
// CHECK-FOURTH-DAG: "{{(.\\/)?}}other.swift"
// CHECK-FOURTH: {{^}$}}
