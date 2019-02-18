// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O  %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT
// RUN: %target-build-swift -O -wmo %s -o %t/a.out2
// RUN: %target-codesign %t/a.out2
// RUN: %target-run %t/a.out2 | %FileCheck %s -check-prefix=CHECK-OUTPUT
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

public func testClassLet(_ f: Foo1) -> Int32 {
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

public class C {}

struct Boo3 {
  //public 
  let Prop0: Int32
  let Prop1: Int32
  fileprivate let Prop2: Int32
  internal let Prop3: Int32

  @inline(__always)
  init(_ f1: C, _ f2: C) {
    self.Prop0 = 0
    self.Prop1 = 1
    self.Prop2 = 2
    self.Prop3 = 3
  }

  init(_ v: C) {
    self.Prop0 = 10
    self.Prop1 = 11
    self.Prop2 = 12
    self.Prop3 = 13
  }
}

// Check that the sum of properties is not folded into a constant.
@inline(never)
func testStructWithMultipleInits( _ boos1: Boo3, _ boos2: Boo3) -> Int32 {
  // count1 = 0 + 1 + 2 + 3 = 6
  // count2 = 10 + 11 + 12 + 13 = 46
  // count1 + count2 = 6 + 46 = 52
  let count1 =  boos1.Prop0 + boos1.Prop1 + boos1.Prop2 + boos1.Prop3
  let count2 =  boos2.Prop0 + boos2.Prop1 + boos2.Prop2 + boos2.Prop3
  return count1 + count2
}

public func testStructWithMultipleInitsAndInlinedInitializer() {
  let things = [C()]
  // This line results in inlining of the initializer Boo3(C, C) and later
  // removal of this initializer by the dead function elimination pass.
  // As a result, only one initializer, Boo3(C) is seen by the Let Properties Propagation
  // pass. This pass may think that there is only one initializer and take the
  // values of let properties assigned there as constants and try to propagate
  // those values into uses. But this is wrong! The pass should be clever enough
  // to detect all stores to the let properties, including those outside of
  // initializers, e.g. inside inlined initializers. And if it detects all such
  // stores it should understand that values of let properties in Boo3 are not
  // statically known constant initializers with the same value and thus
  // cannot be propagated.
  let boos1 = things.map { Boo3($0, C()) }
  let boos2 = things.map(Boo3.init)
  // CHECK-OUTPUT: 52
  print(testStructWithMultipleInits(boos1[0], boos2[0]))
}

testStructWithMultipleInitsAndInlinedInitializer()
