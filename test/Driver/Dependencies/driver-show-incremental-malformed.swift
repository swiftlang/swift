// REQUIRES: shell
// Test that when:
//
// 1. Using -incremental -v -driver-show-incremental, and...
// 2. ...the build record file does not contain valid JSON...
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

// RUN: rm %t/main~buildrecord.swiftdeps && touch %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-MALFORMED %s

// RUN: echo 'foo' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-MALFORMED %s

// CHECK-COARSE-MALFORMED: Incremental compilation has been disabled{{.*}}malformed build record file
// CHECK-COARSE-MALFORMED-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: echo '{version, inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-MISSING-KEY %s

// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-COARSE-MISSING-KEY %s

// CHECK-COARSE-MISSING-KEY: Incremental compilation has been disabled{{.*}}malformed build record file{{.*}}Malformed value for key
// CHECK-COARSE-MISSING-KEY-NOT: Queuing (initial): {compile: main.o <= main.swift}


// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/one-way-with-swiftdeps/output.json %t
// RUN: echo 'var V = a' > %t/main.swift
// RUN: echo 'let a = 34' > %t/other.swift
// RUN: %{python} %S/Inputs/touch.py 443865900 %t/*


// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps

// Create swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies  -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-CREATING-SWIFTDEPS %s
// CHECK-FINE-CREATING-SWIFTDEPS: Disabling incremental build: malformed swift dependencies file

// RUN: touch main.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-INCREMENTAL %s
// CHECK-FINE-INCREMENTAL-NOT: Disabling incremental build
// CHECK-FINE-INCREMENTAL: Queuing (initial): {compile: main.o <= main.swift}

// RUN: rm %t/main~buildrecord.swiftdeps && touch %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-MALFORMED %s

// RUN: echo 'foo' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-MALFORMED %s

// CHECK-FINE-MALFORMED: Incremental compilation has been disabled{{.*}}malformed build record file
// CHECK-FINE-MALFORMED-NOT: Queuing (initial): {compile: main.o <= main.swift}

// RUN: echo '{version, inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-MISSING-KEY %s

// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json 2>&1 | %FileCheck --check-prefix CHECK-FINE-MISSING-KEY %s

// CHECK-FINE-MISSING-KEY: Incremental compilation has been disabled{{.*}}malformed build record file{{.*}}Malformed value for key
// CHECK-FINE-MISSING-KEY-NOT: Queuing (initial): {compile: main.o <= main.swift}
