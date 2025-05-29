// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -playground-high-performance -enable-experimental-feature PlaygroundExtendedCallbacks) | %FileCheck %s
//
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
// CHECK: [{{.*}}] __builtin_log_with_id_extended[a='true' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='5' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='0' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='1' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='2' module: main. file: {{.*/main.swift}}]

var b = true
for i in 0..<3 {
  i
  continue
}
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[b='true' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='0' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='1' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='2' module: main. file: {{.*/main.swift}}]

var c = true
for i in 0..<3 {
  i
  break
}
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[c='true' module: main. file: {{.*/main.swift}}]
// CHECK-NEXT: [{{.*}}] __builtin_log_with_id_extended[='0' module: main. file: {{.*/main.swift}}]
