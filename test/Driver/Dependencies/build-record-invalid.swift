
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/bindings-build-record/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s -check-prefix=CHECK-ALL-BUILT

// CHECK-ALL-BUILT: Handled main.swift
// CHECK-ALL-BUILT: Handled other.swift

// RUN: echo '{version: "bogus", inputs: {"./main.swift": [443865900, 0], "./other.swift": !private [443865900, 0], "./yet-another.swift": !dirty [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: echo 'provides-nominal: garbage' > %t/main.swiftdeps
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s -check-prefix=CHECK-ALL-BUILT
