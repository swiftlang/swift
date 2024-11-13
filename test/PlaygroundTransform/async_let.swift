// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -disable-availability-checking -Xfrontend -playground -o %t/main5a -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -disable-availability-checking -Xfrontend -playground -o %t/main6a -I=%t %t/PlaygroundSupport.o %t/main.swift

// -pc-macro -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -disable-availability-checking -Xfrontend -pc-macro -Xfrontend -playground -o %t/main5b -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -disable-availability-checking -Xfrontend -pc-macro -Xfrontend -playground -o %t/main6b -I=%t %t/PlaygroundSupport.o %t/main.swift

// RUN: %target-codesign %t/main5a
// RUN: %target-codesign %t/main5b
// RUN: %target-codesign %t/main6a
// RUN: %target-codesign %t/main6b

// RUN: %target-run %t/main5a | %FileCheck %s
// RUN: %target-run %t/main5b | %FileCheck %s
// RUN: %target-run %t/main6a | %FileCheck %s
// RUN: %target-run %t/main6b | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

// rdar://76038845
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import PlaygroundSupport

func factorial(_ x : Int) async -> Int {
    if x < 1 {
        return 1
    }
    return await x * factorial(x - 1)
}

async let fac4 = factorial(4)
print(await fac4)

//             return await x * factorial(x - 1)
// CHECK:      [36:{{[[:digit:]]+}}-36:{{[[:digit:]]+}}] __builtin_log[='1']

//             return await x * factorial(x - 1)
// CHECK:      [38:{{[[:digit:]]+}}-38:{{[[:digit:]]+}}] __builtin_log[='1']

//             return await x * factorial(x - 1)
// CHECK:      [38:{{[[:digit:]]+}}-38:{{[[:digit:]]+}}] __builtin_log[='2']

//             return await x * factorial(x - 1)
// CHECK:      [38:{{[[:digit:]]+}}-38:{{[[:digit:]]+}}] __builtin_log[='6']

//             return await x * factorial(x - 1)
// CHECK:      [38:{{[[:digit:]]+}}-38:{{[[:digit:]]+}}] __builtin_log[='24']

//             func factorial(_ x : Int) { ... }
// CHECK-NEXT: [34:{{[[:digit:]]+}}-39:{{[[:digit:]]+}}] __builtin_log_scope_exit

// (actually print the thing)
// CHECK-NEXT: 24

//             print(await fac4)
// CHECK-NEXT: [42:{{[[:digit:]]+}}-42:{{[[:digit:]]+}}] __builtin_postPrint
