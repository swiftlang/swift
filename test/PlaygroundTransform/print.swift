// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
//
// REQUIRES: executable_test

import PlaygroundSupport

var str : String = ""

print(("One", 2))
print("One", to: &str)
print("One", terminator: "\n")
print("One", terminator: "")
print("One", terminator: "\n", to: &str)
print("One", terminator: "", to: &str)

debugPrint(("One", 2))
debugPrint("One", to: &str)
debugPrint("One", terminator: "\n")
debugPrint("One", terminator: "")
debugPrint("One", terminator: "\n", to: &str)
debugPrint("One", terminator: "", to: &str)

// CHECK: [{{.*}}] __builtin_log[str='']
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: One
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: One[{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: "One"
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: "One"[{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint
// CHECK-NEXT: [{{.*}}] __builtin_postPrint

