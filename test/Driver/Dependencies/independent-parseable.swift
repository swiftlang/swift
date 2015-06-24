// RUN: rm -rf %t && cp -r %S/Inputs/independent/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -j1 -parseable-output 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

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

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -j1 -parseable-output 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND: {{^{$}}
// CHECK-SECOND: "kind": "skipped"
// CHECK-SECOND: "name": "compile"
// CHECK-SECOND: ".\/main.swift"
// CHECK-SECOND: {{^}$}}

// RUN: touch -t 201401240006 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -j1 -parseable-output 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s


// RUN: rm -rf %t && cp -r %S/Inputs/independent/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | FileCheck -check-prefix=CHECK-FIRST-MULTI %s

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI: "kind": "began"
// CHECK-FIRST-MULTI: "name": "compile"
// CHECK-FIRST-MULTI: ".\/main.swift"
// CHECK-FIRST-MULTI: {{^}$}}

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI: "kind": "finished"
// CHECK-FIRST-MULTI: "name": "compile"
// CHECK-FIRST-MULTI: "output": "Handled main.swift\n"
// CHECK-FIRST-MULTI: {{^}$}}

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI: "kind": "began"
// CHECK-FIRST-MULTI: "name": "compile"
// CHECK-FIRST-MULTI: ".\/other.swift"
// CHECK-FIRST-MULTI: {{^}$}}

// CHECK-FIRST-MULTI: {{^{$}}
// CHECK-FIRST-MULTI: "kind": "finished"
// CHECK-FIRST-MULTI: "name": "compile"
// CHECK-FIRST-MULTI: "output": "Handled other.swift\n"
// CHECK-FIRST-MULTI: {{^}$}}

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | FileCheck -check-prefix=CHECK-SECOND-MULTI %s

// CHECK-SECOND-MULTI: {{^{$}}
// CHECK-SECOND-MULTI: "kind": "skipped"
// CHECK-SECOND-MULTI: "name": "compile"
// CHECK-SECOND-MULTI: ".\/main.swift"
// CHECK-SECOND-MULTI: {{^}$}}

// CHECK-SECOND-MULTI: {{^{$}}
// CHECK-SECOND-MULTI: "kind": "skipped"
// CHECK-SECOND-MULTI: "name": "compile"
// CHECK-SECOND-MULTI: ".\/other.swift"
// CHECK-SECOND-MULTI: {{^}$}}

// RUN: touch -t 201401240006 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -parseable-output 2>&1 | FileCheck -check-prefix=CHECK-FIRST-MULTI %s

