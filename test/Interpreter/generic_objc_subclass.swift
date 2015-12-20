// RUN: rm -rf %t
// RUN: mkdir -p %t
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-run %t/a.out | FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import ObjCClasses

@objc protocol P {
  func calculatePrice() -> Int
}

class A<T> : HasHiddenIvars, P {
  var first: Int = 16
  var second: T? = nil
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

let aa = A<(Int, Int)>()
let ff = { (aa.x, aa.y, aa.z, aa.t, aa.first, aa.second, aa.third) }

// CHECK: (0, 0, 0, 0, 16, nil, 61)
print(ff())

aa.x = 101
aa.second = (19, 84)
aa.third = 17

// CHECK: (101, 0, 0, 0, 16, Optional((19, 84)), 17)
print(ff())

class B : A<(Int, Int)> {
  override var description: String {
    return "Salmon"
  }

  @nonobjc override func calculatePrice() -> Int {
    return 1675
  }
}

class C : A<(Int, Int)> {
  @nonobjc override var description: String {
    return "Invisible Chicken"
  }

  override func calculatePrice() -> Int {
    return 650
  }
}

// CHECK: 400
// CHECK: 650
print((B() as P).calculatePrice())
print((C() as P).calculatePrice())

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
