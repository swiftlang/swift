// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/bindings-build-record/* %t
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*

// RUN: cd %t && %swiftc_driver -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | %FileCheck %s -check-prefix=MUST-EXEC

// MUST-EXEC-NOT: warning
// MUST-EXEC: inputs: ["./main.swift"], output: {{[{].*[}]}}, condition: run-without-cascading
// MUST-EXEC: inputs: ["./other.swift"], output: {{[{].*[}]}}, condition: run-without-cascading
// MUST-EXEC: inputs: ["./yet-another.swift"], output: {{[{].*[}]}}, condition: run-without-cascading

// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0], "./yet-another.swift": [443865900, 0]}, build_time: [443865901, 0]}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | %FileCheck %s -check-prefix=NO-EXEC

// NO-EXEC: inputs: ["./main.swift"], output: {{[{].*[}]}}, condition: check-dependencies
// NO-EXEC: inputs: ["./other.swift"], output: {{[{].*[}]}}, condition: check-dependencies
// NO-EXEC: inputs: ["./yet-another.swift"], output: {{[{].*[}]}}, condition: check-dependencies


// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": !private [443865900, 0], "./yet-another.swift": !dirty [443865900, 0]}, build_time: [443865901, 0]}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | %FileCheck %s -check-prefix=BUILD-RECORD

// BUILD-RECORD: inputs: ["./main.swift"], output: {{[{].*[}]}}, condition: check-dependencies{{$}}
// BUILD-RECORD: inputs: ["./other.swift"], output: {{[{].*[}]}}, condition: run-without-cascading{{$}}
// BUILD-RECORD: inputs: ["./yet-another.swift"], output: {{[{].*[}]$}}

// RUN: cd %t && %swiftc_driver -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift ./added.swift -incremental -output-file-map %t/output.json 2>&1 > %t/added.txt
// RUN: %FileCheck %s -check-prefix=BUILD-RECORD < %t/added.txt
// RUN: %FileCheck %s -check-prefix=FILE-ADDED < %t/added.txt

// FILE-ADDED: inputs: ["./added.swift"], output: {{[{].*[}]}}, condition: newly-added{{$}}

// RUN: %S/Inputs/touch.py 443865960 %t/main.swift
// RUN: cd %t && %swiftc_driver -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | %FileCheck %s -check-prefix=BUILD-RECORD-PLUS-CHANGE
// BUILD-RECORD-PLUS-CHANGE: inputs: ["./main.swift"], output: {{[{].*[}]}}, condition: run-without-cascading
// BUILD-RECORD-PLUS-CHANGE: inputs: ["./other.swift"], output: {{[{].*[}]}}, condition: run-without-cascading{{$}}
// BUILD-RECORD-PLUS-CHANGE: inputs: ["./yet-another.swift"], output: {{[{].*[}]$}}

// RUN: %S/Inputs/touch.py 443865900 %t/*
// RUN: cd %t && %swiftc_driver -driver-print-bindings ./main.swift ./other.swift -incremental -output-file-map %t/output.json 2>&1 | %FileCheck %s -check-prefix=FILE-REMOVED
// FILE-REMOVED: inputs: ["./main.swift"], output: {{[{].*[}]$}}
// FILE-REMOVED: inputs: ["./other.swift"], output: {{[{].*[}]$}}
// FILE-REMOVED-NOT: yet-another.swift


// RUN: echo '{version: "bogus", inputs: {"./main.swift": [443865900, 0], "./other.swift": !private [443865900, 0], "./yet-another.swift": !dirty [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-print-bindings ./main.swift ./other.swift ./yet-another.swift -incremental -output-file-map %t/output.json 2>&1 | %FileCheck %s -check-prefix=INVALID-RECORD

// INVALID-RECORD-NOT: warning
// INVALID-RECORD: inputs: ["./main.swift"], output: {{[{].*[}]$}}
// INVALID-RECORD: inputs: ["./other.swift"], output: {{[{].*[}]$}}
// INVALID-RECORD: inputs: ["./yet-another.swift"], output: {{[{].*[}]$}}
