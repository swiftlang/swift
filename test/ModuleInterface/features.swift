// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -swift-version 5 -module-name FeatureTest -emit-module-interface-path - -enable-library-evolution -enable-experimental-concurrency %s | %FileCheck %s
// REQUIRES: concurrency

// REQUIRES: concurrency

// Ensure that when we emit a Swift interface that makes use of new features,
// the uses of those features are guarded by appropriate #if's that allow older
// compilers to skip over the uses of newer features.

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

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: @_marker public protocol MP2 : FeatureTest.MP {
// CHECK-NEXT: }
// CHECK-NEXT: #endif
@_marker public protocol MP2: MP { }

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: public protocol MP3
// CHECK-NEXT: }
public protocol MP3: MP { }
// CHECK-NEXT: #endif

// CHECK: class OldSchool {
public class OldSchool: MP {
  // CHECK: #if compiler(>=5.3) && $AsyncAwait
  // CHECK-NEXT: takeClass()
  // CHECK-NEXT: #endif
  public func takeClass() async { }
}

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: extension Array : FeatureTest.MP where Element : FeatureTest.MP {
extension Array: FeatureTest.MP where Element : FeatureTest.MP { }
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: extension OldSchool : Swift.UnsafeConcurrentValue {
extension OldSchool: UnsafeConcurrentValue { }
// CHECK-NEXT: }
// CHECK-NEXT: #endif

// CHECK: #if compiler(>=5.3) && $AsyncAwait
// CHECK-NEXT: func runSomethingSomewhere
// CHECK-NEXT: #endif
public func runSomethingSomewhere(body: () async -> Void) { }

// CHECK: #if compiler(>=5.3) && $Actors
// CHECK-NEXT: func stage
// CHECK-NEXT: #endif
public func stage(with actor: MyActor) { }

// CHECK-NOT: extension FeatureTest.MyActor : Swift.ConcurrentValue

// CHECK: #if compiler(>=5.3) && $MarkerProtocol
// CHECK-NEXT: extension FeatureTest.OldSchool : FeatureTest.MP {
// CHECK-NEXT: #endif

