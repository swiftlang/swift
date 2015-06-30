/// other ==> main

// RUN: rm -rf %t && cp -r %S/Inputs/one-way/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -emit-module-path %t/master.swiftmodule -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift
// CHECK-FIRST: Produced master.swiftmodule

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -emit-module-path %t/master.swiftmodule -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: warning
// CHECK-SECOND-NOT: Handled
// CHECK-SECOND: Produced master.swiftmodule
