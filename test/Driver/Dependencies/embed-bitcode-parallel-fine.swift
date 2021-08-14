// Windows doesn't support parallel execution yet
// XFAIL: windows
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/fake-build-for-bitcode.py" -output-file-map %t/output.json -incremental ./main.swift ./other.swift -embed-bitcode -module-name main -j1 -parseable-output >%t/first.json  2>&1

// RUN: %FileCheck -check-prefix=CHECK-FIRST %s <%t/first.json
// RUN: %FileCheck -check-prefix=CHECK-FIRST %s <%t/first.json
// RUN: %FileCheck -check-prefix=CHECK-FIRST %s <%t/first.json
// RUN: %FileCheck -check-prefix=CHECK-FIRST %s <%t/first.json
// RUN: %FileCheck -check-prefix=CHECK-FIRST %s <%t/first.json

// CHECK-FIRST-NOT: warning

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "{{(.\\/)?}}main.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "finished"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "output"{{ ?}}: "Handled main.swift\n"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "{{(.\\/)?}}other.swift"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "finished"
// CHECK-FIRST-DAG: "name"{{ ?}}: "compile"
// CHECK-FIRST-DAG: "output"{{ ?}}: "Handled other.swift\n"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-DAG: "name"{{ ?}}: "backend"
// CHECK-FIRST-DAG: "{{(.\\/)?}}main.o"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "finished"
// CHECK-FIRST-DAG: "name"{{ ?}}: "backend"
// CHECK-FIRST-DAG: "output"{{ ?}}: "Produced main.o\n"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAG: "kind"{{ ?}}: "began"
// CHECK-FIRST-DAG: "name"{{ ?}}: "backend"
// CHECK-FIRST-DAG: "{{(.\\/)?}}other.o"
// CHECK-FIRST: {{^}$}}

// CHECK-FIRST: {{^{$}}
// CHECK-FIRST-DAT: "kind"{{ ?}}: "finished"
// CHECK-FIRST-DAT: "name"{{ ?}}: "backend"
// CHECK-FIRST-DAT: "output"{{ ?}}: "Produced other.o\n"
// CHECK-FIRST: {{^}$}}


// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/fake-build-for-bitcode.py" -output-file-map %t/output.json -incremental ./main.swift ./other.swift -embed-bitcode -module-name main -j2 -parseable-output 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-DAG: "kind"{{ ?}}: "began"
// CHECK-SECOND-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-DAG: "{{(.\\/)?}}main.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND-NOT: finished

// CHECK-SECOND-DAG: "kind"{{ ?}}: "began"
// CHECK-SECOND-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-DAG: "{{(.\\/)?}}other.swift"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND-NOT: began

// CHECK-SECOND-DAG: "kind"{{ ?}}: "finished"
// CHECK-SECOND-DAG: "name"{{ ?}}: "compile"
// CHECK-SECOND-DAG: "output"{{ ?}}: "Handled {{other.swift|main.swift}}\n"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND-DAG: "kind"{{ ?}}: "began"
// CHECK-SECOND-DAG: "name"{{ ?}}: "backend"
// CHECK-SECOND-DAG: "{{(.\\/)?}}{{other.o|main.o}}"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND-DAG: "kind"{{ ?}}: "finished"
// CHECK-SECOND-DAG: "name"{{ ?}}: "backend"
// CHECK-SECOND-DAG: "output"{{ ?}}: "Produced {{other.o|main.o}}\n"
// CHECK-SECOND: {{^}$}}

// CHECK-SECOND-NOT: "skipped"
