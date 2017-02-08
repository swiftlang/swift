// Test that when:
//
// 1. Using -incremental -v -driver-show-incremental, and...
// 2. ...the build record file does not contain valid JSON...
//
// ...then the driver prints a message indicating that incremental compilation
// is disabled.


// RUN: rm -rf %t && cp -r %S/Inputs/one-way/ %t
// RUN: %S/Inputs/touch.py 443865900 %t/*

// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %S/Inputs/update-dependencies.py -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-INCREMENTAL %s
// CHECK-INCREMENTAL-NOT: Incremental compilation has been disabled
// CHECK-INCREMENTAL: Queuing main.swift (initial)

// RUN: rm %t/main~buildrecord.swiftdeps && touch %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %S/Inputs/update-dependencies.py -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-MALFORMED %s

// RUN: echo 'foo' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %S/Inputs/update-dependencies.py -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-MALFORMED %s

// CHECK-MALFORMED: Incremental compilation has been disabled{{.*}}malformed build record file
// CHECK-MALFORMED-NOT: Queuing main.swift (initial)

// RUN: echo '{version, inputs: {"./main.swift": [443865900, 0], "./other.swift": [443865900, 0]}}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %S/Inputs/update-dependencies.py -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-MISSING-KEY %s

// RUN: echo '{version: "'$(%swiftc_driver_plain -version | head -n1)'", inputs}' > %t/main~buildrecord.swiftdeps
// RUN: cd %t && %swiftc_driver -driver-use-frontend-path %S/Inputs/update-dependencies.py -g -c ./main.swift ./other.swift -module-name main -incremental -v -driver-show-incremental -output-file-map %t/output.json | %FileCheck --check-prefix CHECK-MISSING-KEY %s

// CHECK-MISSING-KEY: Incremental compilation has been disabled{{.*}}malformed build record file{{.*}}Malformed value for key
// CHECK-MISSING-KEY-NOT: Queuing main.swift (initial)

