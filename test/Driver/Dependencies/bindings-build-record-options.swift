// RUN: rm -rf %t && cp -r %S/Inputs/bindings-build-record/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC

// MUST-EXEC-NOT: warning
// MUST-EXEC: inputs: ["./main.swift"], output: {{[{].*[}]$}}
// MUST-EXEC: inputs: ["./other.swift"], output: {{[{].*[}]$}}
// MUST-EXEC: inputs: ["./yet-another.swift"], output: {{[{].*[}]$}}

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-use-frontend-path %S/Inputs/update-dependencies.py ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC

// NO-EXEC: inputs: ["./main.swift"], output: {{[{].*[}]}}, condition: check-dependencies
// NO-EXEC: inputs: ["./other.swift"], output: {{[{].*[}]}}, condition: check-dependencies
// NO-EXEC: inputs: ["./yet-another.swift"], output: {{[{].*[}]}}, condition: check-dependencies


// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings -serialize-diagnostics ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -O -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-use-frontend-path %S/Inputs/update-dependencies.py ./main.swift ./other.swift ./yet-another.swift -incremental -O -output-file-map %t/output.json
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -O -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -O -serialize-diagnostics -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -Onone -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-use-frontend-path %S/Inputs/update-dependencies.py ./main.swift ./other.swift ./yet-another.swift -incremental -Onone -output-file-map %t/output.json
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -Onone -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-use-frontend-path %S/Inputs/update-dependencies.py ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -I. -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-use-frontend-path %S/Inputs/update-dependencies.py ./main.swift ./other.swift ./yet-another.swift -incremental -I. -output-file-map %t/output.json
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -I. -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -I. -I/ -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-use-frontend-path %S/Inputs/update-dependencies.py ./main.swift ./other.swift ./yet-another.swift -incremental -I. -I/ -output-file-map %t/output.json
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -I. -I/ -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC

// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -I. -DDEBUG -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=MUST-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-use-frontend-path %S/Inputs/update-dependencies.py ./main.swift ./other.swift ./yet-another.swift -incremental -I. -DDEBUG -output-file-map %t/output.json
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -I. -DDEBUG -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC
// RUN: cd %t && %swiftc_driver -c -module-name main -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -DDEBUG -I. -output-file-map %t/output.json 2>&1 | FileCheck %s -check-prefix=NO-EXEC
