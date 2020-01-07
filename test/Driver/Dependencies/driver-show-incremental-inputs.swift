// REQUIRES: shell
// Test that when:
//
// 1. Using -incremental -v -driver-show-incremental, and...
// 2. ...the inputs passed to the Swift compiler version differ from the ones
//    used in the original compilation...
//
// ...then the driver prints a message indicating that incremental compilation
// is disabled.


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-with-swiftdeps/* %t
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -disable-fine-grained-dependencies -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-INCREMENTAL %s
// CHECK-COARSE-INCREMENTAL-NOT: Incremental compilation has been disabled
// CHECK-COARSE-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -disable-fine-grained-dependencies -c ./main.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-INPUTS-MISMATCH %s
// CHECK-COARSE-INPUTS-MISMATCH: Incremental compilation has been disabled{{.*}}inputs
// CHECK-COARSE-INPUTS-MISMATCH: ./other.swift
// CHECK-COARSE-INPUTS-MISMATCH-NOT: Queuing (initial): {compile: main.o <= main.swift}



// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/one-way-with-swiftdeps/output.json %t
// RUN: echo 'var V = 23' >%t/main.swift
// RUN: echo 'func foo() {}' >%t/other.swift
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps

// Create swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies  -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-CREATING-SWIFTDEPS %s
// CHECK-FINE-CREATING-SWIFTDEPS: Disabling incremental build: malformed swift dependencies file

// RUN: touch main.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | tee /tmp/1|%FileCheck --check-prefix CHECK-FINE-INCREMENTAL %s
// CHECK-FINE-INCREMENTAL-NOT: Disabling incremental build
// CHECK-FINE-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c ./main.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 |tee /tmp/2| %FileCheck --check-prefix CHECK-FINE-INPUTS-MISMATCH %s
// CHECK-FINE-INPUTS-MISMATCH: Incremental compilation has been disabled{{.*}}inputs
// CHECK-FINE-INPUTS-MISMATCH: ./other.swift
// CHECK-FINE-INPUTS-MISMATCH-NOT: Queuing (initial): {compile: main.o <= main.swift}

