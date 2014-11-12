// RUN: rm -rf %t && cp -r %S/Inputs/simple/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -emit-reference-dependencies ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift

// RUN: touch -t 201401240005 %t/other.o
// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -emit-reference-dependencies ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled main.swift
// CHECK-SECOND: Handled other.swift
// CHECK-SECOND-NOT: Handled main.swift

// RUN: rm %t/other.swiftdeps
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -emit-reference-dependencies ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s

// The order is reversed here because:
// 1. We see that main.swift isn't dirty itself.
// 2. We try to load other.swift's swiftdeps and fail.
// 3. And then we reschedule main.swift.
// CHECK-THIRD: Handled other.swift
// CHECK-THIRD: Handled main.swift
