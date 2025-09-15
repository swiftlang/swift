// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -emit-ir -parse-as-library -module-name main | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -parse-as-library -module-name main -o %t/main %target-rpath(%t) 
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency

struct G<T> {
  // CHECK-LL: @"$s4main1GV19theMutatingFunctionyqd__qd__YalFTu" = hidden global %swift.async_func_pointer
  // CHECK-LL: define hidden swift{{(tail)?}}cc void @"$s4main1GV19theMutatingFunctionyqd__qd__YalF"(
  mutating func theMutatingFunction<U>(_ u: U) async -> U {
    return u
  }
}

func theMutatingCaller<T, U>(_ g: inout G<T>, _ u: U) async -> U {
  return await g.theMutatingFunction(u)
}

@main struct Main {
  static func main() async {
    var g = G<Int>()
    let i = await theMutatingCaller(&g, 3)
    // CHECK: 3
    print(i)
  }
}
