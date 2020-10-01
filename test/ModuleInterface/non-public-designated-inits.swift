// Note: This test has a client: non-public-designated-inits-client.swift

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck %s -emit-module-interface-path %t/Module.swiftinterface -module-name Module -enable-library-evolution
// RUN: %FileCheck %s < %t/Module.swiftinterface

// CHECK: @_hasMissingDesignatedInitializers open class A {
open class A {
  // This is a non-public designated init, which means the convenience
  // init should not be inheritable.
  init() {}

  // CHECK-NEXT: public init(_: Swift.Int)
  public init(_: Int) {}

  // CHECK-NEXT: convenience public init(hi: ())
  public convenience init(hi: ()) { self.init() }

// CHECK: }
}
