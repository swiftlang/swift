// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-build-swift -O  %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s -check-prefix=CHECK-OUTPUT
// REQUIRES: executable_test

// Check that in optimized builds the compiler generates correct code for
// initializations of let properties, which is assigned multiple times inside
// initializers.

public class Foo1 {
  internal let Prop1: Int32
  internal let Prop2: Int32
  // Initialize Prop3 as part of declaration.
  internal let Prop3: Int32 = 20
  internal let Prop4: Int32

  @inline(never)
  init(_ count: Int32) {
    // Initialize Prop4 unconditionally and only once.
    Prop4 = 300
    // There are two different assignments to Prop1 and Prop2
    // on different branches of the if-statement.
    if count < 2 {
      // Initialize Prop1 and Prop2 conditionally.
      // Use other properties in the definition of Prop1 and Prop2.
      Prop1 = 5
      Prop2 = 10 - Prop1 + Prop4 - Prop3
    } else {
       // Initialize Prop1 and Prop2 conditionally.
      // Use other properties in the definition of Prop1 and Prop2.
      Prop1 = 100
      Prop2 = 200 + Prop1 - Prop4 - Prop3
    }
  }
}

public func testClassLet(f: Foo1) -> Int32 {
  return f.Prop1 + f.Prop2 + f.Prop3 + f.Prop4
}

// Prop1 = 5, Prop2 = (10-5+300-20) = 285, Prop3 = 20, Prop4 = 300
// Hence Prop1 + Prop2 + Prop3 + Prop4 = 610
// CHECK-OUTPUT: 610
print(testClassLet(Foo1(1)))

// Prop1 = 100, Prop2 = (200+100-300-20) = -20, Prop3 = 20, Prop4 = 300
// Hence Prop1 + Prop2 + Prop3 + Prop4 = 610
// CHECK-OUTPUT: 400
print(testClassLet(Foo1(10)))

