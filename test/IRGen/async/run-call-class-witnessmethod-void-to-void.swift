// RUN: %empty-directory(%t)
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -parse-as-library -emit-ir -module-name main | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift  -target %target-swift-5.1-abi-triple -g %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency

func printGeneric<T>(_ t: T) {
  print(t)
}

protocol P {
  func f() async
}

// CHECK: entering call_f
// CHECK: entering f
// CHECK: X
// CHECK: main.X
// CHECK: exiting f
// CHECK: exiting call_f

// CHECK-LL: @"$s4main1PPAAE1fyyYaFTu" = hidden global %swift.async_func_pointer
// CHECK-LL: @"$s4main1XCAA1PA2aDP1fyyYaFTWTu" = internal global %swift.async_func_pointer
// CHECK-LL: @"$s4main6call_fyyxYaAA1PRzlFTu" = hidden global %swift.async_func_pointer

extension P {
  // CHECK-LL: define hidden swift{{(tail)?}}cc void @"$s4main1PPAAE1fyyYaF"(
  func f() async {
    print("entering f")
    printGeneric(Self.self)
    printGeneric(self)
    print("exiting f")
  }
}

// CHECK-LL: define internal swift{{(tail)?}}cc void @"$s4main1XCAA1PA2aDP1fyyYaFTW"(
extension X : P {}

// CHECK-LL: define hidden swift{{(tail)?}}cc void @"$s4main6call_fyyxYaAA1PRzlF"(
func call_f<T : P>(_ t: T) async {
  print("entering call_f")
  await t.f()
  print("exiting call_f")
}

class X {}

@main struct Main {
  static func main() async {
    let x = X()
    await call_f(x)
  }
}
