/// main <==> other

// RUN: rm -rf %t && cp -r %S/Inputs/mutual/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s

// CHECK-THIRD: Handled other.swift
// CHECK-THIRD: Handled main.swift

// RUN: touch -t 201401240006 %t/main.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s
