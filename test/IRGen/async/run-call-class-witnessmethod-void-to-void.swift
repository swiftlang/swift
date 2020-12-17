// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -emit-ir | %FileCheck %s --check-prefix=CHECK-LL
// RUN: %target-build-swift -Xfrontend -enable-experimental-concurrency %s -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib

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

// CHECK-LL: @"$s4main1PPAAE1fyyYFTu" = hidden global %swift.async_func_pointer
// CHECK-LL: @"$s4main6call_fyyxYAA1PRzlFTu" = hidden global %swift.async_func_pointer
// CHECK-LL: @"$s4main1XCAA1PA2aDP1fyyYFTWTu" = internal global %swift.async_func_pointer

extension P {
  // CHECK-LL: define hidden swiftcc void @"$s4main1PPAAE1fyyYF"(%swift.task* {{%[0-9]+}}, %swift.executor* {{%[0-9]+}}, %swift.context* {{%[0-9]+}}) {{#[0-9]*}} {
  func f() async {
    print("entering f")
    printGeneric(Self.self)
    printGeneric(self)
    print("exiting f")
  }
}

// CHECK-LL: define internal swiftcc void @"$s4main1XCAA1PA2aDP1fyyYFTW"(%swift.task* {{%[0-9]+}}, %swift.executor* {{%[0-9]+}}, %swift.context* {{%[0-9]+}}) {{#[0-9]*}} {
extension X : P {}

// CHECK-LL: define hidden swiftcc void @"$s4main6call_fyyxYAA1PRzlF"(%swift.task* {{%[0-9]+}}, %swift.executor* {{%[0-9]+}}, %swift.context* {{%[0-9]+}}) {{#[0-9]*}} {
func call_f<T : P>(_ t: T) async {
  print("entering call_f")
  await t.f()
  print("exiting call_f")
}

class X {}

runAsyncAndBlock {
  let x = X()
  await call_f(x)
}
