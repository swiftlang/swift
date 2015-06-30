/// other ==> main

// RUN: rm -rf %t && cp -r %S/Inputs/one-way/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: mkdir %t/bin
// RUN: cp %S/Inputs/fake-ld.py %t/bin/ld
// RUN: cp %S/Inputs/update-dependencies.py %t/bin/

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %t/bin/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift
// CHECK-FIRST: Linked main

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %t/bin/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: warning
// CHECK-SECOND-NOT: Handled
// CHECK-SECOND: Linked main
