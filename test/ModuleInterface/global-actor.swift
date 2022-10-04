// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: concurrency

// Global actors are no longer protected with a feature guard.
// CHECK-NOT: $GlobalActors

// CHECK: @globalActor public struct GlobalActor {
@available(SwiftStdlib 5.1, *)
@globalActor public struct GlobalActor {
  public actor Actor { }
  public static let shared = Actor()
}

// CHECK: @Test.GlobalActor public func funcBoundToGlobalActor()
@available(SwiftStdlib 5.1, *)
@GlobalActor public func funcBoundToGlobalActor() { }

// CHECK: public func funcWithParameterBoundToGlobalActor(_ x: Test.ClassBoundToGlobalActor)
@available(SwiftStdlib 5.1, *)
public func funcWithParameterBoundToGlobalActor(_ x: ClassBoundToGlobalActor) { }

// CHECK: @Test.GlobalActor public class ClassBoundToGlobalActor
@available(SwiftStdlib 5.1, *)
@GlobalActor public class ClassBoundToGlobalActor { }

// CHECK: extension Test.ClassBoundToGlobalActor
@available(SwiftStdlib 5.1, *)
extension ClassBoundToGlobalActor {
  public func someMethod() { }
}

// CHECK: @Test.GlobalActor public class DerivedFromClassBoundToGlobalActor : Test.ClassBoundToGlobalActor
@available(SwiftStdlib 5.1, *)
public class DerivedFromClassBoundToGlobalActor: ClassBoundToGlobalActor {}

// CHECK: public class NoActorClass
@available(SwiftStdlib 5.1, *)
public class NoActorClass {
  // CHECK: @Test.GlobalActor public var varBoundToGlobalActor: Swift.Int
  @GlobalActor public var varBoundToGlobalActor: Int
  
  // CHECK: @Test.GlobalActor public init()
  @GlobalActor public init() {
    self.varBoundToGlobalActor = 0
  }

  // CHECK: @Test.GlobalActor public func methodBoundToGlobalActor()
  @GlobalActor public func methodBoundToGlobalActor() { }
}

// CHECK: extension Test.GlobalActor : _Concurrency.GlobalActor {}
// CHECK: extension Test.ClassBoundToGlobalActor : Swift.Sendable {}
