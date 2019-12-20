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

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-INCREMENTAL %s
// CHECK-INCREMENTAL-NOT: Incremental compilation has been disabled
// CHECK-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -whole-module-optimization -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-WMO %s
// CHECK-WMO: Incremental compilation has been disabled{{.*}}whole module optimization
// CHECK-WMO-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -embed-bitcode -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-BITCODE %s
// CHECK-BITCODE: Incremental compilation has been disabled{{.*}}LLVM IR bitcode
// CHECK-BITCODE-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -whole-module-optimization -embed-bitcode -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-WMO-AND-BITCODE %s
// CHECK-WMO-AND-BITCODE: Incremental compilation has been disabled{{.*}}whole module optimization
// CHECK-WMO-AND-BITCODE-NOT: Incremental compilation has been disabled
// CHECK-WMO-AND-BITCODE-NOT: Queuing (initial): {compile: main.o <= main.swift}

