// RUN: %target-swift-frontend %s -g -enable-experimental-feature Embedded -emit-ir | %FileCheck %s
// REQUIRES: swift_feature_Embedded

// 'dynamic' is ignored in Embedded Swift, so 'foo' is not dynamically
// replaceable (see dynamic-ignored.swift).

// CHECK-LABEL: define {{.*}} @"$e4main3fooyS2iF"
// CHECK-NOT:     call ptr @swift_getFunctionReplacement
// CHECK:         ret
dynamic func foo(_ i: Int) -> Int {
  return i
}

public func test(_ i: Int) -> Int {
  return foo(i)
}
