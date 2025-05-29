// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -parse-as-library -module-name main -emit-ir | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency

// CHECK-LL: @genericToVoidTu =
// CHECK-LL: define hidden swift{{(tail)?}}cc void @genericToVoid(
@_silgen_name("genericToVoid")
func genericToVoid<T>(_ t: T) async {
  print(t) // CHECK: 922337203685477580

}

@_silgen_name("test_case")
func test_case() async {
  await genericToVoid(922337203685477580 as Int64)
}

@main struct Main {
  static func main() async {
    await test_case()
  }
}
