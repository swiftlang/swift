// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Test
// RUN: %FileCheck %s < %t.swiftinterface

// REQUIRES: concurrency

// Global actors are no longer protected with a feature guard.
// CHECK-NOT: $GlobalActors

// CHECK: @globalActor public struct GlobalActor {
@globalActor public struct GlobalActor {
  public actor Actor { }
  public static let shared = Actor()
}

// CHECK: @Test.GlobalActor public func funcBoundToGlobalActor()
@GlobalActor public func funcBoundToGlobalActor() { }

// CHECK: public func funcWithParameterBoundToGlobalActor(_ x: Test.ClassBoundToGlobalActor)
public func funcWithParameterBoundToGlobalActor(_ x: ClassBoundToGlobalActor) { }

// CHECK: @_hasMissingDesignatedInitializers @Test.GlobalActor public class ClassBoundToGlobalActor
@GlobalActor public class ClassBoundToGlobalActor { }

// CHECK: extension Test.ClassBoundToGlobalActor
extension ClassBoundToGlobalActor {
  public func someMethod() { }
}

// CHECK: @_inheritsConvenienceInitializers @_hasMissingDesignatedInitializers @Test.GlobalActor public class DerivedFromClassBoundToGlobalActor : Test.ClassBoundToGlobalActor
public class DerivedFromClassBoundToGlobalActor: ClassBoundToGlobalActor {}

// CHECK: public class NoActorClass
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
