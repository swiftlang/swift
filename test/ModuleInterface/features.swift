// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -swift-version 5 -module-name FeatureTest -emit-module-interface-path %t/FeatureTest.swiftinterface -enable-library-evolution -enable-experimental-concurrency %s
// RUN: %FileCheck %s < %t/FeatureTest.swiftinterface --check-prefix CHECK

// Make sure we can parse the file without concurrency enabled

// CHECK: #if compiler(>=5.3) && $Actors
// CHECK-NEXT: actor {{.*}} MyActor
// CHECK: }
// CHECK-NEXT: #endif
public actor class MyActor {
}

// CHECK: #if compiler(>=5.3) && $Actors
// CHECK-NEXT: extension MyActor
public extension MyActor {
  // CHECK-NOT: $Actors
  // CHECK: testFunc
  func testFunc() async { }
  // CHECK: }
  // CHECK-NEXT: #endif
}

// CHECK: #if compiler(>=5.3) && $AsyncAwait
// CHECK-NEXT: globalAsync
// CHECK-NEXT: #endif
public func globalAsync() async { }

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: public protocol MP
@_marker public protocol MP { }

// CHECK: class OldSchool
public class OldSchool: MP {
  // CHECK: #if compiler(>=5.3) && $AsyncAwait
  // CHECK-NEXT: takeClass()
  // CHECK-NEXT: #endif
  public func takeClass() async { }
}

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: extension Swift.Array : MP where Element : MP
extension Array: FeatureTest.MP where Element : FeatureTest.MP { }
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: extension FeatureTest.MyActor : Swift.ConcurrentValue {}
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: extension FeatureTest.OldSchool : Swift.UnsafeConcurrentValue
extension OldSchool: UnsafeConcurrentValue {
  public func method() { }
}
