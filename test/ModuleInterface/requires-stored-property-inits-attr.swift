// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface)
// RUN: %FileCheck %s < %t.swiftinterface

// CHECK: @requires_stored_property_inits public class RequiresStoredPropertyInits
@requires_stored_property_inits
public class RequiresStoredPropertyInits {
  // CHECK: final public let a: Swift.Int{{$}}
  public let a: Int = 0

  public init() {}
}
