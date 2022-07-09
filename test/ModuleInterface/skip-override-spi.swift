// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Foo.swiftinterface) %s -module-name Foo
// RUN: %target-swift-typecheck-module-from-interface(%t/Foo.swiftinterface) -module-name Foo
// RUN: %FileCheck %s < %t/Foo.swiftinterface

// CHECK: public class HideyHole
public class HideyHole {
  // CHECK-NOT: public init()
  @_spi(Private) public init() {}
}

// CHECK: @_inheritsConvenienceInitializers public class StashyCache : Foo.HideyHole
public class StashyCache: HideyHole {
  // CHECK: public init()
}
