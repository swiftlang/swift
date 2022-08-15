// Note: This test has a client: non-public-designated-inits-client.swift

// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Module.swiftinterface) %S/non-public-designated-inits.swift -module-name Module
// RUN: %target-swift-typecheck-module-from-interface(%t/Module.swiftinterface) -module-name Module
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
