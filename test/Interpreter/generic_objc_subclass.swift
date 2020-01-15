// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import ObjCClasses

@objc protocol P {
  func calculatePrice() -> Int
}

protocol PP {
  func calculateTaxes() -> Int
}

//
// Generic subclass of an @objc class
//

class A<T> : HasHiddenIvars, P {
  var first: Int = 16
  var second: T?
  var third: Int = 61

  override var description: String {
    return "Grilled artichokes"
  }

  func calculatePrice() -> Int {
    return 400
  }
}

let a = A<Int>()

// CHECK: Grilled artichokes
// CHECK: Grilled artichokes
print(a.description)
print((a as NSObject).description)

let f = { (a.x, a.y, a.z, a.t, a.first, a.second, a.third) }

// CHECK: (0, 0, 0, 0, 16, nil, 61)
print(f())

// CHECK: (25, 225, 255, 2255, 16, nil, 61)
a.x = 25
a.y = 225
a.z = 255
a.t = 2255
print(f())

// CHECK: (36, 225, 255, 2255, 16, nil, 61)
a.x = 36
print(f())

// CHECK: (36, 225, 255, 2255, 16, Optional(121), 61)
a.second = 121
print(f())

//
// Instantiate the class with a different set of generic parameters
//

let aa = A<(Int, Int)>()
let ff = { (aa.x, aa.y, aa.z, aa.t, aa.first, aa.second, aa.third) }

// CHECK: (0, 0, 0, 0, 16, nil, 61)
print(ff())

aa.x = 101
aa.second = (19, 84)
aa.third = 17

// CHECK: (101, 0, 0, 0, 16, Optional((19, 84)), 17)
print(ff())

//
// Concrete subclass of generic subclass of @objc class
//

class B : A<(Int, Int)> {
  override var description: String {
    return "Salmon"
  }

  @nonobjc override func calculatePrice() -> Int {
    return 1675
  }
}

class BB : B {}

class C : A<(Int, Int)>, PP {
  @nonobjc override var description: String {
    return "Invisible Chicken"
  }

  override func calculatePrice() -> Int {
    return 650
  }

  func calculateTaxes() -> Int {
    return 110
  }
}

// CHECK: 400
// CHECK: 400
// CHECK: 650
// CHECK: 110
print((BB() as P).calculatePrice())
print((B() as P).calculatePrice())
print((C() as P).calculatePrice())
print((C() as PP).calculateTaxes())

// CHECK: Salmon
// CHECK: Grilled artichokes
print((B() as NSObject).description)
print((C() as NSObject).description)

let b = B()
let g = { (b.x, b.y, b.z, b.t, b.first, b.second, b.third) }

// CHECK: (0, 0, 0, 0, 16, nil, 61)
print(g())

b.x = 101
b.second = (19, 84)
b.third = 17

// CHECK: (101, 0, 0, 0, 16, Optional((19, 84)), 17)
print(g())

//
// Generic subclass of @objc class without any generically-sized members
//

class FixedA<T> : HasHiddenIvars, P {
  var first: Int = 16
  var second: [T] = []
  var third: Int = 61

  override var description: String {
    return "Grilled artichokes"
  }

  func calculatePrice() -> Int {
    return 400
  }
}

let fixedA = FixedA<Int>()

// CHECK: Grilled artichokes
// CHECK: Grilled artichokes
print(fixedA.description)
print((fixedA as NSObject).description)

let fixedF = { (fixedA.x, fixedA.y, fixedA.z, fixedA.t, fixedA.first, fixedA.second, fixedA.third) }

// CHECK: (0, 0, 0, 0, 16, [], 61)
print(fixedF())

// CHECK: (25, 225, 255, 2255, 16, [], 61)
fixedA.x = 25
fixedA.y = 225
fixedA.z = 255
fixedA.t = 2255
print(fixedF())

// CHECK: (36, 225, 255, 2255, 16, [], 61)
fixedA.x = 36
print(fixedF())

// CHECK: (36, 225, 255, 2255, 16, [121], 61)
fixedA.second = [121]
print(fixedF())

//
// Instantiate the class with a different set of generic parameters
//

let fixedAA = FixedA<(Int, Int)>()
let fixedFF = { (fixedAA.x, fixedAA.y, fixedAA.z, fixedAA.t, fixedAA.first, fixedAA.second, fixedAA.third) }

// CHECK: (0, 0, 0, 0, 16, [], 61)
print(fixedFF())

fixedAA.x = 101
fixedAA.second = [(19, 84)]
fixedAA.third = 17

// CHECK: (101, 0, 0, 0, 16, [(19, 84)], 17)
print(fixedFF())

//
// Concrete subclass of generic subclass of @objc class
// without any generically-sized members
//

class FixedB : FixedA<Int> {
  override var description: String {
    return "Salmon"
  }

  override func calculatePrice() -> Int {
    return 1675
  }
}

// CHECK: 675
print((FixedB() as P).calculatePrice())

// CHECK: Salmon
print((FixedB() as NSObject).description)

let fixedB = FixedB()
let fixedG = { (fixedB.x, fixedB.y, fixedB.z, fixedB.t, fixedB.first, fixedB.second, fixedB.third) }

// CHECK: (0, 0, 0, 0, 16, [], 61)
print(fixedG())

fixedB.x = 101
fixedB.second = [19, 84]
fixedB.third = 17

// CHECK: (101, 0, 0, 0, 16, [19, 84], 17)
print(fixedG())

// Problem with field alignment in direct generic subclass of NSObject -
// <https://bugs.swift.org/browse/SR-2586>
public class PandorasBox<T>: NSObject {
    final public var value: T

    public init(_ value: T) {
        // Uses ConstantIndirect access pattern
        self.value = value
    }
}

let c = PandorasBox(30)
// CHECK: 30
// Uses ConstantDirect access pattern
print(c.value)

// Super method calls from a generic subclass of an @objc class
class HasDynamicMethod : NSObject {
  @objc dynamic class func funkyTown() {
    print("Here we are with \(self)")
  }
}

class GenericOverrideOfDynamicMethod<T> : HasDynamicMethod {
  override class func funkyTown() {
    print("Hello from \(self) with T = \(T.self)")
    super.funkyTown()
    print("Goodbye from \(self) with T = \(T.self)")
  }
}

class ConcreteOverrideOfDynamicMethod : GenericOverrideOfDynamicMethod<Int> {
  override class func funkyTown() {
    print("Hello from \(self)")
    super.funkyTown()
    print("Goodbye from \(self)")
  }
}

// CHECK: Hello from ConcreteOverrideOfDynamicMethod
// CHECK: Hello from ConcreteOverrideOfDynamicMethod with T = Int
// CHECK: Here we are with ConcreteOverrideOfDynamicMethod
// CHECK: Goodbye from ConcreteOverrideOfDynamicMethod with T = Int
// CHECK: Goodbye from ConcreteOverrideOfDynamicMethod
ConcreteOverrideOfDynamicMethod.funkyTown()

class Foo {}
class Bar {}
class DependOnAlignOf<T> : HasHiddenIvars2 {
  var first = Foo()
  var second = Bar()
  var third: T?
}

let ad = DependOnAlignOf<Double>()
let ai = DependOnAlignOf<Int>()

do {
  let fd = { (ad.x, ad.first, ad.second, ad.third) }
  let fi = { (ai.x, ai.first, ai.second, ai.third) }

  // CHECK: (nil, a.Foo, a.Bar, nil)
  print(fd())

  // CHECK: (nil, a.Foo, a.Bar, nil)
  print(fi())
}

// Same as above, but there's another class in between the
// Objective-C class and us
class HasHiddenIvars3 : HasHiddenIvars2 { }

class AlsoDependOnAlignOf<T> : HasHiddenIvars3 {
  var first = Foo()
  var second = Bar()
  var third: T?
}

do {
  let ad = AlsoDependOnAlignOf<Double>()
  let ai = AlsoDependOnAlignOf<Int>()

  let fd = { (ad.x, ad.first, ad.second, ad.third) }
  let fi = { (ai.x, ai.first, ai.second, ai.third) }

  // CHECK: (nil, a.Foo, a.Bar, nil)
  print(fd())

  // CHECK: (nil, a.Foo, a.Bar, nil)
  print(fi())
}
