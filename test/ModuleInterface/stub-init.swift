// RUN: %target-swift-frontend -typecheck -swift-version 5 -emit-module-interface-path - -module-name test %s | %FileCheck %s

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

