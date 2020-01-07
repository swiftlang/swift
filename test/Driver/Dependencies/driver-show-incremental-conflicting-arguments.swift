// REQUIRES: shell
// Test that when:
//
// 1. Using -incremental -v -driver-show-incremental, but...
// 2. ...options that disable incremental compilation, such as whole module
//    optimization or bitcode embedding are specified...
//
// ...then the driver prints a message indicating that incremental compilation
// is disabled. If both are specified, the driver should only print one message.


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-with-swiftdeps/* %t
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-INCREMENTAL %s
// CHECK-COARSE-INCREMENTAL-NOT: Incremental compilation has been disabled
// CHECK-COARSE-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -whole-module-optimization -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-WMO %s
// CHECK-COARSE-WMO: Incremental compilation has been disabled{{.*}}whole module optimization
// CHECK-COARSE-WMO-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -embed-bitcode -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-BITCODE %s
// CHECK-COARSE-BITCODE: Incremental compilation has been disabled{{.*}}LLVM IR bitcode
// CHECK-COARSE-BITCODE-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -whole-module-optimization -embed-bitcode -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-WMO-AND-BITCODE %s
// CHECK-COARSE-WMO-AND-BITCODE: Incremental compilation has been disabled{{.*}}whole module optimization
// CHECK-COARSE-WMO-AND-BITCODE-NOT: Incremental compilation has been disabled
// CHECK-COARSE-WMO-AND-BITCODE-NOT: Queuing (initial): {compile: main.o <= main.swift}



// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/one-way-with-swiftdeps/output.json %t
// RUN: echo 'var V = a' >%t/main.swift
// RUN: echo 'let a = 23' >%t/other.swift
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps

// Create swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies  -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-FINE-CREATING-SWIFTDEPS %s

// CHECK-FINE-CREATING-SWIFTDEPS: Disabling incremental build: malformed swift dependencies file

// RUN: touch %t/main.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies  -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-INCREMENTAL %s
// CHECK-FINE-INCREMENTAL-NOT: Disabling incremental build
// CHECK-FINE-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies  -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -whole-module-optimization -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-WMO %s
// CHECK-FINE-WMO: Incremental compilation has been disabled{{.*}}whole module optimization
// CHECK-FINE-WMO-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies  -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -embed-bitcode -output-file-map %t/output.json  2>&1 | %FileCheck --check-prefix CHECK-FINE-BITCODE %s
// CHECK-FINE-BITCODE: Incremental compilation has been disabled{{.*}}LLVM IR bitcode
// CHECK-FINE-BITCODE-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies  -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -whole-module-optimization -embed-bitcode -output-file-map %t/output.json  2>&1 | %FileCheck --check-prefix CHECK-FINE-WMO-AND-BITCODE %s
// CHECK-FINE-WMO-AND-BITCODE: Incremental compilation has been disabled{{.*}}whole module optimization
// CHECK-FINE-WMO-AND-BITCODE-NOT: Incremental compilation has been disabled
// CHECK-FINE-WMO-AND-BITCODE-NOT: Queuing (initial): {compile: main.o <= main.swift}

