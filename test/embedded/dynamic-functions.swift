// RUN: %target-swift-frontend %s -g -enable-experimental-feature Embedded -emit-ir | %FileCheck %s
// REQUIRES: swift_feature_Embedded


// CHECK-LABEL: define {{.*}} @"$e4main3fooyS2iF"
// CHECK:         call ptr @swift_getFunctionReplacement
// CHECK:         ret
dynamic func foo(_ i: Int) -> Int {
  return i
}

public func test(_ i: Int) -> Int {
  return foo(i)
}
