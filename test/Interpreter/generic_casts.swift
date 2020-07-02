// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Onone %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck --check-prefix CHECK --check-prefix CHECK-ONONE %s
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

protocol P {}
struct PS: P {}
enum PE: P {}
class PC: P {}
class PCSub: PC {}

// `is` checks
func nongenericAnyIsPConforming(type: Any.Type) -> Bool {
  // `is P.Type` tests whether the argument conforms to `P`
  // Note:  this can only be true for a concrete type, never a protocol
  return type is P.Type
}
func nongenericAnyIsPSubtype(type: Any.Type) -> Bool {
  // `is P.Protocol` tests whether the argument is a subtype of `P`
  // In particular, it is true for `P.self`
  return type is P.Protocol
}
func nongenericAnyIsPAndAnyObjectConforming(type: Any.Type) -> Bool {
  return type is (P & AnyObject).Type
}
func nongenericAnyIsPAndPCSubConforming(type: Any.Type) -> Bool {
  return type is (P & PCSub).Type
}
func genericAnyIs<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  // If we're testing against a runtime that doesn't have the fix this tests,
  // just pretend we got it right.
  if #available(macOS 10.15.4, iOS 13.4, watchOS 6.2, tvOS 13.4, *) {
    return type is T.Type
  } else {
    return expected
  }
}
// `as?` checks
func nongenericAnyAsConditionalPConforming(type: Any.Type) -> Bool {
  return (type as? P.Type) != nil
}
func nongenericAnyAsConditionalPSubtype(type: Any.Type) -> Bool {
  return (type as? P.Protocol) != nil
}
func nongenericAnyAsConditionalPAndAnyObjectConforming(type: Any.Type) -> Bool {
  return (type as? (P & AnyObject).Type) != nil
}
func nongenericAnyAsConditionalPAndPCSubConforming(type: Any.Type) -> Bool {
  return (type as? (P & PCSub).Type) != nil
}
func genericAnyAsConditional<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  // If we're testing against a runtime that doesn't have the fix this tests,
  // just pretend we got it right.
  if #available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *) {
    return (type as? T.Type) != nil
  } else {
    return expected
  }
}
// `as!` checks
func blackhole<T>(_ : T) { }

func nongenericAnyAsUnconditionalPConforming(type: Any.Type) -> Bool {
  blackhole(type as! P.Type)
  return true
}
func nongenericAnyAsUnconditionalPSubtype(type: Any.Type) -> Bool {
  blackhole(type as! P.Protocol)
  return true
}
func nongenericAnyAsUnconditionalPAndAnyObjectConforming(type: Any.Type) -> Bool {
  blackhole(type as! (P & AnyObject).Type)
  return true
}
func nongenericAnyAsUnconditionalPAndPCSubConforming(type: Any.Type) -> Bool {
  blackhole(type as! (P & PCSub).Type)
  return true
}
func genericAnyAsUnconditional<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  if #available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *) {
    blackhole(type as! T.Type)
  }
  return true
}

// CHECK-LABEL: casting types to protocols with generics:
print("casting types to protocols with generics:")
print(nongenericAnyIsPConforming(type: P.self)) // CHECK: false
print(nongenericAnyIsPSubtype(type: P.self)) // CHECK: true
print(genericAnyIs(type: P.self, to: P.self, expected: true)) // CHECK: true
print(nongenericAnyIsPConforming(type: PS.self)) // CHECK: true
print(genericAnyIs(type: PS.self, to: P.self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyIsPConforming(type: PE.self)) // CHECK: true
print(genericAnyIs(type: PE.self, to: P.self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyIsPConforming(type: PC.self)) // CHECK: true
print(genericAnyIs(type: PC.self, to: P.self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyIsPConforming(type: PCSub.self)) // CHECK: true
print(genericAnyIs(type: PCSub.self, to: P.self, expected: true)) // CHECK-ONONE: true

// CHECK-LABEL: conditionally casting types to protocols with generics:
print("conditionally casting types to protocols with generics:")
print(nongenericAnyAsConditionalPConforming(type: P.self)) // CHECK: false
print(nongenericAnyAsConditionalPSubtype(type: P.self)) // CHECK: true
print(genericAnyAsConditional(type: P.self, to: P.self, expected: true)) // CHECK: true
print(nongenericAnyAsConditionalPConforming(type: PS.self)) // CHECK: true
print(genericAnyAsConditional(type: PS.self, to: P.self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyAsConditionalPConforming(type: PE.self)) // CHECK: true
print(genericAnyAsConditional(type: PE.self, to: P.self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyAsConditionalPConforming(type: PC.self)) // CHECK: true
print(genericAnyAsConditional(type: PC.self, to: P.self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyAsConditionalPConforming(type: PCSub.self)) // CHECK: true
print(genericAnyAsConditional(type: PCSub.self, to: P.self, expected: true)) // CHECK-ONONE: true

// CHECK-LABEL: unconditionally casting types to protocols with generics:
print("unconditionally casting types to protocols with generics:")
//print(nongenericAnyAsUnconditionalPConforming(type: P.self)) // expected to trap
print(nongenericAnyAsUnconditionalPSubtype(type: P.self)) // CHECK: true
print(genericAnyAsUnconditional(type: P.self, to: P.self, expected: true)) // CHECK: true
print(nongenericAnyAsUnconditionalPConforming(type: PS.self)) // CHECK: true
print(genericAnyAsUnconditional(type: PS.self, to: P.self, expected: true)) // CHECK: true
print(nongenericAnyAsUnconditionalPConforming(type: PE.self)) // CHECK: true
print(genericAnyAsUnconditional(type: PE.self, to: P.self, expected: true)) // CHECK: true
print(nongenericAnyAsUnconditionalPConforming(type: PC.self)) // CHECK: true
print(genericAnyAsUnconditional(type: PC.self, to: P.self, expected: true)) // CHECK: true
print(nongenericAnyAsUnconditionalPConforming(type: PCSub.self)) // CHECK: true
print(genericAnyAsUnconditional(type: PCSub.self, to: P.self, expected: true)) // CHECK: true

// CHECK-LABEL: casting types to protocol & AnyObject existentials:
print("casting types to protocol & AnyObject existentials:")
print(nongenericAnyIsPAndAnyObjectConforming(type: PS.self)) // CHECK: false
print(genericAnyIs(type: PS.self, to: (P & AnyObject).self, expected: false)) // CHECK: false
print(nongenericAnyIsPAndAnyObjectConforming(type: PE.self)) // CHECK: false
print(genericAnyIs(type: PE.self, to: (P & AnyObject).self, expected: false)) // CHECK: false
print(nongenericAnyIsPAndAnyObjectConforming(type: PC.self)) // CHECK: true
print(genericAnyIs(type: PC.self, to: (P & AnyObject).self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyIsPAndAnyObjectConforming(type: PCSub.self)) // CHECK: true
print(genericAnyIs(type: PCSub.self, to: (P & AnyObject).self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyAsConditionalPAndAnyObjectConforming(type: PS.self)) // CHECK: false
print(genericAnyAsConditional(type: PS.self, to: (P & AnyObject).self, expected: false)) // CHECK: false
print(nongenericAnyAsConditionalPAndAnyObjectConforming(type: PE.self)) // CHECK: false
print(genericAnyAsConditional(type: PE.self, to: (P & AnyObject).self, expected: false)) // CHECK: false
print(nongenericAnyAsConditionalPAndAnyObjectConforming(type: PC.self)) // CHECK: true
print(genericAnyAsConditional(type: PC.self, to: (P & AnyObject).self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyAsConditionalPAndAnyObjectConforming(type: PCSub.self)) // CHECK: true
print(genericAnyAsConditional(type: PCSub.self, to: (P & AnyObject).self, expected: true)) // CHECK-ONONE: true

// CHECK-LABEL: casting types to protocol & class existentials:
print("casting types to protocol & class existentials:")
print(nongenericAnyIsPAndPCSubConforming(type: PS.self)) // CHECK: false
print(genericAnyIs(type: PS.self, to: (P & PCSub).self, expected: false)) // CHECK: false
print(nongenericAnyIsPAndPCSubConforming(type: PE.self)) // CHECK: false
print(genericAnyIs(type: PE.self, to: (P & PCSub).self, expected: false)) // CHECK: false
//print(nongenericAnyIsPAndPCSubConforming(type: PC.self)) // CHECK-SR-11565: false -- FIXME: reenable this when SR-11565 is fixed
print(genericAnyIs(type: PC.self, to: (P & PCSub).self, expected: false)) // CHECK: false
print(nongenericAnyIsPAndPCSubConforming(type: PCSub.self)) // CHECK: true
print(genericAnyIs(type: PCSub.self, to: (P & PCSub).self, expected: true)) // CHECK-ONONE: true
print(nongenericAnyAsConditionalPAndPCSubConforming(type: PS.self)) // CHECK: false
print(genericAnyAsConditional(type: PS.self, to: (P & PCSub).self, expected: false)) // CHECK: false
print(nongenericAnyAsConditionalPAndPCSubConforming(type: PE.self)) // CHECK: false
print(genericAnyAsConditional(type: PE.self, to: (P & PCSub).self, expected: false)) // CHECK: false
//print(nongenericAnyAsConditionalPAndPCSubConforming(type: PC.self)) // CHECK-SR-11565: false -- FIXME: reenable this when SR-11565 is fixed
print(genericAnyAsConditional(type: PC.self, to: (P & PCSub).self, expected: false)) // CHECK: false
print(nongenericAnyAsConditionalPAndPCSubConforming(type: PCSub.self)) // CHECK: true
print(genericAnyAsConditional(type: PCSub.self, to: (P & PCSub).self, expected: true)) // CHECK-ONONE: true


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
