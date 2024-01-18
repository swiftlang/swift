// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/independent-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "{{(.\\/)?}}main.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST: "kind"{{ ?}}: "finished"
// CHECK-FIRST: "name"{{ ?}}: "compile"
// CHECK-FIRST: "output"{{ ?}}: "Handled main.swift{{(\\r)?}}\n"
// CHECK-FIRST: {{^}$}}

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND-DAG: "kind"{{ ?}}: "skipped"
// CHECK-SECOND-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-DAG: "{{(.\\/)?}}main.swift"
// CHECK-SECOND: {{^}$}}

// Don't mess with the priors
// RUN: touch -t 201401240006 %t/*.{swift,swiftdeps,json}
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/independent-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST-MULTI %s

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-MULTI-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-MULTI-DAG: "{{(.\\/)?}}main.swift"
// CHECK-FIRST-MULTI: {{^}$}}

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI-DAG: "kind"{{ ?}}: "finished"
// CHECK-FIRST-MULTI-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-MULTI-DAG: "output"{{ ?}}: "Handled main.swift{{(\\r)?}}\n"
// CHECK-FIRST-MULTI: {{^}$}}

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-MULTI-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-MULTI-DAG: "{{(.\\/)?}}other.swift"
// CHECK-FIRST-MULTI: {{^}$}}

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI-DAG: "kind"{{ ?}}: "finished"
// CHECK-FIRST-MULTI-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-MULTI-DAG: "output"{{ ?}}: "Handled other.swift{{(\\r)?}}\n"
// CHECK-FIRST-MULTI: {{^}$}}

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-SECOND-MULTI %s

// CHECK-SECOND-MULTI: {{^{$}}
// CHECK-SECOND-MULTI-DAG: "kind"{{ ?}}: "skipped"
// CHECK-SECOND-MULTI-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-MULTI-DAG: "{{(.\\/)?}}main.swift"
// CHECK-SECOND-MULTI: {{^}$}}

// CHECK-SECOND-MULTI: {{^{$}}
// CHECK-SECOND-MULTI-DAG: "kind"{{ ?}}: "skipped"
// CHECK-SECOND-MULTI-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-MULTI-DAG: "{{(.\\/)?}}other.swift"
// CHECK-SECOND-MULTI: {{^}$}}

// RUN: touch -t 201401240006 %t/*.{swift,swiftdeps,json}
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-FIRST-MULTI %s

