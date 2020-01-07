// REQUIRES: shell
// Test that when:
//
// 1. Using -incremental -v -driver-show-incremental, and...
// 2. ...the arguments passed to the Swift compiler version differ from the ones
//    used in the original compilation...
//
// ...then the driver prints a message indicating that incremental compilation
// is disabled.


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/one-way-with-swiftdeps/* %t
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-INCREMENTAL %s
// CHECK-COARSE-INCREMENTAL-NOT: Incremental compilation has been disabled
// CHECK-COARSE-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-ARGS-MISMATCH %s
// CHECK-COARSE-ARGS-MISMATCH: Incremental compilation has been disabled{{.*}}different arguments
// CHECK-COARSE-ARGS-MISMATCH-NOT: Queuing (initial): {compile: main.o <= main.swift}




// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/one-way-with-swiftdeps/output.json %t
// RUN: echo 'var V = a' > %t/main.swift
// RUN: echo 'let a = 34' > %t/other.swift
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps

// create good swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json

// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*
// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-FINE-INCREMENTAL %s
// CHECK-FINE-INCREMENTAL-NOT: Disabling incremental build
// CHECK-FINE-INCREMENTAL: Queuing {{.*}}: {compile: main.o <= main.swift}

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-FINE-ARGS-MISMATCH %s
// CHECK-FINE-ARGS-MISMATCH: Incremental compilation has been disabled{{.*}}different arguments
// CHECK-FINE-ARGS-MISMATCH-NOT: Queuing (initial): {compile: main.o <= main.swift}


