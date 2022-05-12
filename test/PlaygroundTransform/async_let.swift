// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -Xfrontend -playground -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s

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
// CHECK:      [22:{{[[:digit:]]+}}-22:{{[[:digit:]]+}}] __builtin_log[='1']

//             return await x * factorial(x - 1)
// CHECK:      [24:{{[[:digit:]]+}}-24:{{[[:digit:]]+}}] __builtin_log[='1']

//             return await x * factorial(x - 1)
// CHECK:      [24:{{[[:digit:]]+}}-24:{{[[:digit:]]+}}] __builtin_log[='2']

//             return await x * factorial(x - 1)
// CHECK:      [24:{{[[:digit:]]+}}-24:{{[[:digit:]]+}}] __builtin_log[='6']

//             return await x * factorial(x - 1)
// CHECK:      [24:{{[[:digit:]]+}}-24:{{[[:digit:]]+}}] __builtin_log[='24']

//             func factorial(_ x : Int) { ... }
// CHECK-NEXT: [20:{{[[:digit:]]+}}-25:{{[[:digit:]]+}}] __builtin_log_scope_exit

// (actually print the thing)
// CHECK-NEXT: 24

//             print(await fac4)
// CHECK-NEXT: [28:{{[[:digit:]]+}}-28:{{[[:digit:]]+}}] __builtin_postPrint
