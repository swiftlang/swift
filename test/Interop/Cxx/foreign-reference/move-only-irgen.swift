// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-experimental-cxx-interop -validate-tbd-against-ir=none -disable-llvm-verify -Xcc -fignore-exceptions -disable-availability-checking | %FileCheck %s

import MoveOnly


// CHECK-LABEL: define {{.*}}swiftcc void @"$s4main4testyyF"

// CHECK: [[X:%.*]] = alloca ptr

// CHECK: [[CREATED:%.*]] = call ptr @{{_ZN8MoveOnly6createEv|"\?create\@MoveOnly\@\@SAPEAU1\@XZ"}}()
// CHECK: store ptr [[CREATED]], ptr [[X]]

// CHECK: call i32 @{{_ZNK8MoveOnly4testEv|"\?test\@MoveOnly\@\@QEBAHXZ"}}(ptr [[CREATED]])

// CHECK: ret void

public func test() {
  var x = MoveOnly.create()
  _ = x.test()
}
