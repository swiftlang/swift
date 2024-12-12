// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -disable-availability-checking -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -disable-availability-checking -Xfrontend -playground) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -disable-availability-checking -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -disable-availability-checking -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
//
// REQUIRES: executable_test
// REQUIRES: concurrency
//
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
// CHECK:      [20:{{[[:digit:]]+}}-20:{{[[:digit:]]+}}] __builtin_log[='1']

//             return await x * factorial(x - 1)
// CHECK:      [22:{{[[:digit:]]+}}-22:{{[[:digit:]]+}}] __builtin_log[='1']

//             return await x * factorial(x - 1)
// CHECK:      [22:{{[[:digit:]]+}}-22:{{[[:digit:]]+}}] __builtin_log[='2']

//             return await x * factorial(x - 1)
// CHECK:      [22:{{[[:digit:]]+}}-22:{{[[:digit:]]+}}] __builtin_log[='6']

//             return await x * factorial(x - 1)
// CHECK:      [22:{{[[:digit:]]+}}-22:{{[[:digit:]]+}}] __builtin_log[='24']

//             func factorial(_ x : Int) { ... }
// CHECK-NEXT: [18:{{[[:digit:]]+}}-23:{{[[:digit:]]+}}] __builtin_log_scope_exit

// (actually print the thing)
// CHECK-NEXT: 24

//             print(await fac4)
// CHECK-NEXT: [26:{{[[:digit:]]+}}-26:{{[[:digit:]]+}}] __builtin_postPrint
