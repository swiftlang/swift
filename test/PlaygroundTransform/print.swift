// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

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

// CHECK: [{{.*}}] $builtin_log[str='']
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: One
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: One[{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: ("One", 2)
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: "One"
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: "One"[{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint
// CHECK-NEXT: [{{.*}}] $builtin_postPrint

