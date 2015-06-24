// other ==> main ==> yet-another

// RUN: rm -rf %t && cp -r %S/Inputs/chained/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift
// CHECK-FIRST: Handled yet-another.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s

// CHECK-THIRD: Handled other.swift
// CHECK-THIRD-DAG: Handled main.swift
// CHECK-THIRD-DAG: Handled yet-another.swift

// RUN: touch -t 201401240007 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./other.swift ./main.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s

// RUN: touch -t 201401240008 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./yet-another.swift ./other.swift ./main.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s

// RUN: touch -t 201401240009 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./other.swift ./yet-another.swift ./main.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s
