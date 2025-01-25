// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/FeatureTest.swiftinterface) %s -module-name FeatureTest -target %target-swift-5.1-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t/FeatureTest.swiftinterface) -module-name FeatureTest -target %target-swift-5.1-abi-triple
// RUN: %FileCheck %s \
// RUN:   --implicit-check-not "\$AsyncAwait" \
// RUN:   --implicit-check-not "\$Actors" \
// RUN:   --implicit-check-not "\$MarkerProtocol" \
// RUN:   --implicit-check-not "\$Sendable" \
// RUN:   --implicit-check-not "\$InheritActorContext" \
// RUN:   --implicit-check-not "\$UnsafeInheritExecutor" \
// RUN:   --implicit-check-not "\$NoAsyncAvailability" \
// RUN:   --implicit-check-not "\$UnavailableFromAsync" \
// RUN:   < %t/FeatureTest.swiftinterface

// REQUIRES: concurrency

// Ensure that when we emit a Swift interface that makes use of new features,
// the uses of those features are guarded by appropriate #if's that allow older
// compilers to skip over the uses of newer features.

// Some feature gaurds are retired when the first compiler that supports the
// feature is old enough. The --implicit-check-not arguments to FileCheck above
// verify that those guards no longer pollute the emitted interface.

// CHECK:      public actor MyActor
// CHECK:        @_semantics("defaultActor") nonisolated final public var unownedExecutor: _Concurrency.UnownedSerialExecutor {
// CHECK-NEXT:     get
// CHECK-NEXT:   }
// CHECK-NEXT: }
public actor MyActor {
}

// CHECK:     extension FeatureTest.MyActor
public extension MyActor {
  // CHECK:     testFunc
  func testFunc() async { }
  // CHECK: }
}

// CHECK:     globalAsync
public func globalAsync() async { }

// CHECK:      @_marker public protocol MP {
// CHECK-NEXT: }
@_marker public protocol MP { }

// CHECK:      @_marker public protocol MP2 : FeatureTest.MP {
// CHECK-NEXT: }
@_marker public protocol MP2: MP { }

// CHECK:      public protocol MP3 : AnyObject, FeatureTest.MP {
// CHECK-NEXT: }
public protocol MP3: AnyObject, MP { }

// CHECK:      extension FeatureTest.MP2 {
// CHECK-NEXT: func inMP2
extension MP2 {
  public func inMP2() { }
}

// CHECK: class OldSchool : FeatureTest.MP {
public class OldSchool: MP {
  // CHECK:     takeClass()
  public func takeClass() async { }
}

// CHECK: class OldSchool2 : FeatureTest.MP {
public class OldSchool2: MP {
  // CHECK:     takeClass()
  public func takeClass() async { }
}

// CHECK: @rethrows public protocol RP
@rethrows public protocol RP {
  func f() throws -> Bool
}

// CHECK: public struct UsesRP {
public struct UsesRP {
  // CHECK:  public var value: (any FeatureTest.RP)? {
  // CHECK-NEXT:  get
  public var value: RP? {
    nil
  }
}

// CHECK: public struct IsRP
public struct IsRP: RP {
  // CHECK-NEXT: public func f()
  public func f() -> Bool { }

  // CHECK-NEXT: public var isF:
  // CHECK-NEXT:   get
  public var isF: Bool {
    f()
  }
}

// CHECK: public func acceptsRP
public func acceptsRP<T: RP>(_: T) { }

// CHECK:     extension Swift.Array : FeatureTest.MP where Element : FeatureTest.MP {
extension Array: FeatureTest.MP where Element : FeatureTest.MP { }
// CHECK: }

// CHECK:     extension FeatureTest.OldSchool : Swift.UnsafeSendable {
extension OldSchool: UnsafeSendable { }
// CHECK-NEXT: }


// CHECK:     func runSomethingSomewhere
public func runSomethingSomewhere(body: () async -> Void) { }

// CHECK:     func runSomethingConcurrently(body: @Sendable () ->
public func runSomethingConcurrently(body: @Sendable () -> Void) { }

// CHECK:     func stage
public func stage(with actor: MyActor) { }

// CHECK:     func asyncIsh
public func asyncIsh(@_inheritActorContext operation: @Sendable @escaping () async -> Void) { }

// CHECK:     @_unsafeInheritExecutor public func unsafeInheritExecutor() async
@_unsafeInheritExecutor
public func unsafeInheritExecutor() async {}


// CHECK:       @_unavailableFromAsync(message: "Test") public func unavailableFromAsyncFunc()
@_unavailableFromAsync(message: "Test")
public func unavailableFromAsyncFunc() { }

// CHECK:      @available(*, noasync, message: "Test")
// CHECK-NEXT: public func noAsyncFunc()
@available(*, noasync, message: "Test")
public func noAsyncFunc() { }

// CHECK-NOT: extension FeatureTest.MyActor : Swift.Sendable
