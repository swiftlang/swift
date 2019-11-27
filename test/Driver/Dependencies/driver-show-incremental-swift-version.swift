// REQUIRES: shell
// Test that when:
//
// 1. Using -incremental -v -driver-show-incremental, and...
// 2. ...the Swift compiler version used to perform the incremental
//    compilation differs the original compilation...
//
// ...then the driver prints a message indicating that incremental compilation
// is disabled.


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-with-swiftdeps/* %t
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*

// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-INCREMENTAL %s
// CHECK-INCREMENTAL-NOT: Incremental compilation has been disabled
// CHECK-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: echo '{version: "bogus", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-VERSION-MISMATCH %s
// CHECK-VERSION-MISMATCH: Incremental compilation has been disabled{{.*}}compiler version mismatch
// CHECK-VERSION-MISMATCH: Compiling with:
// CHECK-VERSION-MISMATCH: Previously compiled with: bogus
// CHECK-VERSION-MISMATCH-NOT: Queuing (initial): {compile: main.o <= main.swift}

