// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks -o %t/main5a -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks -o %t/main6a -I=%t %t/PlaygroundSupport.o %t/main.swift

// -pc-macro -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks -o %t/main5b -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks -o %t/main6b -I=%t %t/PlaygroundSupport.o %t/main.swift

// RUN: %target-codesign %t/main5a
// RUN: %target-codesign %t/main5b
// RUN: %target-codesign %t/main6a
// RUN: %target-codesign %t/main6b

// RUN: %target-run %t/main5a | %FileCheck %s
// RUN: %target-run %t/main5b | %FileCheck %s
// RUN: %target-run %t/main6a | %FileCheck %s
// RUN: %target-run %t/main6b | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_PlaygroundExtendedCallbacks

import PlaygroundSupport

var a = true
if (a) {
  5
} else {
  7
}

for i in 0..<3 {
  i
}
// CHECK: [{{.*}}] __builtin_log_with_id_extended[a='true' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='5' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='0' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='1' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='2' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]

var b = true
for i in 0..<3 {
  i
  continue
}
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[b='true' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='0' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='1' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='2' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]

var c = true
for i in 0..<3 {
  i
  break
}
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[c='true' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='0' module: main{{.*}}. file: {{.*/main[\d]?.swift}}]
