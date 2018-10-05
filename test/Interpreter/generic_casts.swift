// RUN: %target-run-simple-swift | %FileCheck %s
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-codesign %t/a.out.optimized
// RUN: %target-run %t/a.out.optimized | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/19648117 Needs splitting objc parts out

#if canImport(Foundation)
import Foundation
#endif

func allToInt<T>(_ x: T) -> Int {
  return x as! Int
}

func allToIntOrZero<T>(_ x: T) -> Int {
  if x is Int {
    return x as! Int
  }
  return 0
}

func anyToInt(_ x: Any) -> Int {
  return x as! Int
}

func anyToIntOrZero(_ x: Any) -> Int {
  if x is Int {
    return x as! Int
  }
  return 0
}

protocol Class : class {}

class C : Class {

  func print() { Swift.print("C!") }
}
class D : C {
  override func print() { Swift.print("D!") }
}

class E : C {
  override func print() { Swift.print("E!") }
}

class X : Class {
}

func allToC<T>(_ x: T) -> C {
  return x as! C
}

func allToCOrE<T>(_ x: T) -> C {
  if x is C {
    return x as! C
  }
  return E()
}

func anyToC(_ x: Any) -> C {
  return x as! C
}

func anyToCOrE(_ x: Any) -> C {
  if x is C {
    return x as! C
  }
  return E()
}

func allClassesToC<T : Class>(_ x: T) -> C {
  return x as! C
}

func allClassesToCOrE<T : Class>(_ x: T) -> C {
  if x is C {
    return x as! C
  }
  return E()
}

func anyClassToC(_ x: Class) -> C {
  return x as! C
}

func anyClassToCOrE(_ x: Class) -> C {
  if x is C {
    return x as! C
  }
  return E()
}

func allToAll<T, U>(_ t: T, _: U.Type) -> Bool {
  return t is U
}

func allMetasToAllMetas<T, U>(_: T.Type, _: U.Type) -> Bool {
  return T.self is U.Type
}

print(allToInt(22)) // CHECK: 22
print(anyToInt(44)) // CHECK: 44
allToC(C()).print() // CHECK: C!
allToC(D()).print() // CHECK: D!
anyToC(C()).print() // CHECK: C!
anyToC(D()).print() // CHECK: D!
allClassesToC(C()).print() // CHECK: C!
allClassesToC(D()).print() // CHECK: D!
anyClassToC(C()).print() // CHECK: C!
anyClassToC(D()).print() // CHECK: D!

print(allToIntOrZero(55)) // CHECK: 55
print(allToIntOrZero("fifty-five")) // CHECK: 0
print(anyToIntOrZero(88)) // CHECK: 88
print(anyToIntOrZero("eighty-eight")) // CHECK: 0
allToCOrE(C()).print() // CHECK: C!
allToCOrE(D()).print() // CHECK: D!
allToCOrE(143).print() // CHECK: E!
allToCOrE(X()).print() // CHECK: E!
anyToCOrE(C()).print() // CHECK: C!
anyToCOrE(D()).print() // CHECK: D!
anyToCOrE(143).print() // CHECK: E!
anyToCOrE(X()).print() // CHECK: E!
allClassesToCOrE(C()).print() // CHECK: C!
allClassesToCOrE(D()).print() // CHECK: D!
allClassesToCOrE(X()).print() // CHECK: E!
anyClassToCOrE(C()).print() // CHECK: C!
anyClassToCOrE(D()).print() // CHECK: D!
anyClassToCOrE(X()).print() // CHECK: E!

// CHECK-LABEL: type comparisons:
print("type comparisons:\n")
print(allMetasToAllMetas(Int.self, Int.self)) // CHECK: true
print(allMetasToAllMetas(Int.self, Float.self)) // CHECK: false
print(allMetasToAllMetas(C.self, C.self)) // CHECK: true
print(allMetasToAllMetas(D.self, C.self)) // CHECK: true
print(allMetasToAllMetas(C.self, D.self)) // CHECK: false
print(C.self is D.Type) // CHECK: false
print((D.self as C.Type) is D.Type) // CHECK: true

let t: Any.Type = type(of: 1 as Any)
print(t is Int.Type) // CHECK: true
print(t is Float.Type) // CHECK: false
print(t is C.Type) // CHECK: false

let u: Any.Type = type(of: (D() as Any))
print(u is C.Type) // CHECK: true
print(u is D.Type) // CHECK: true
print(u is E.Type) // CHECK: false
print(u is Int.Type) // CHECK: false

// FIXME: Can't spell AnyObject.Protocol
// CHECK-LABEL: AnyObject casts:
print("AnyObject casts:")
print(allToAll(C(), AnyObject.self)) // CHECK: true

// On Darwin, the object will be the ObjC-runtime-class object;
// out of Darwin, this should not succeed.
print(allToAll(type(of: C()), AnyObject.self)) 
// CHECK-objc: true
// CHECK-native: false

// Bridging
// NSNumber on Darwin, __SwiftValue on Linux.
print(allToAll(0, AnyObject.self)) // CHECK: true

// This will get bridged using __SwiftValue.
struct NotBridged { var x: Int }
print(allToAll(NotBridged(x: 0), AnyObject.self)) // CHECK: true

#if canImport(Foundation)
// This requires Foundation (for NSCopying):
print(allToAll(NotBridged(x: 0), NSCopying.self)) // CHECK-objc: true
#endif

// On Darwin, these casts fail (intentionally) even though __SwiftValue does
// technically conform to these protocols through NSObject.
// Off Darwin, it should not conform at all.
print(allToAll(NotBridged(x: 0), CustomStringConvertible.self)) // CHECK: false
print(allToAll(NotBridged(x: 0), (AnyObject & CustomStringConvertible).self)) // CHECK: false

#if canImport(Foundation)
// This requires Foundation (for NSArray):
//
// rdar://problem/19482567
//

func swiftOptimizesThisFunctionIncorrectly() -> Bool {
    let anArray = [] as NSArray

    if let whyThisIsNeverExecutedIfCalledFromFunctionAndNotFromMethod = anArray as? [NSObject] {
        return true
    }
    
    return false
}

let result = swiftOptimizesThisFunctionIncorrectly()
print("Bridge cast result: \(result)") // CHECK-NEXT-objc: Bridge cast result: true
#endif
