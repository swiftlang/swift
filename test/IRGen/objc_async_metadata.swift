// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -enable-experimental-concurrency %s -emit-ir | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: concurrency
// REQUIRES: objc_interop


import Foundation

// CHECK: [[ENCODE_ASYNC_STRING:@.*]] = private unnamed_addr constant [28 x i8] c"v24@0:8@?<v@?@\22NSString\22>16\00"
// CHECK: [[ENCODE_ASYNC_THROWS_STRING:@.*]] = private unnamed_addr constant [38 x i8] c"v24@0:8@?<v@?@\22NSString\22@\22NSError\22>16\00"

// CHECK: @_INSTANCE_METHODS__TtC19objc_async_metadata7MyClass = internal constant
// CHECK-SAME: methodWithCompletionHandler:
// CHECK-SAME: [[ENCODE_ASYNC_STRING]]
// CHECK-SAME: throwingMethodWithCompletionHandler:
// CHECK-SAME: [[ENCODE_ASYNC_THROWS_STRING]]
class MyClass: NSObject {
  @objc func method() async -> String { "" }
  @objc func throwingMethod() async throws -> String { "" }
}
