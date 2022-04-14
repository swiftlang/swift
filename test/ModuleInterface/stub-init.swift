// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name test
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name test
// RUN: %FileCheck %s < %t.swiftinterface

public class Base {
  public init(x: Int) {}
}

public class Derived: Base {
  public init(z: Int) {
    super.init(x: z)
  }
}

// CHECK-LABEL: public class Base {
// CHECK-NEXT:    public init(x: Swift.Int)
// CHECK-NEXT:    {{(@objc )?}}deinit
// CHECK-NEXT: }

// CHECK-LABEL: public class Derived : test.Base {
// CHECK-NEXT:    public init(z: Swift.Int)
// CHECK-NEXT:    {{(@objc )?}}deinit
// CHECK-NEXT: }

