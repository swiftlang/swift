// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Onone %s -o %t/a.out
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/a.out.optimized
//
// RUN: %target-run %t/a.out | %FileCheck --check-prefix CHECK %s
// RUN: %target-run %t/a.out.optimized | %FileCheck --check-prefix CHECK %s

// RUN: %target-build-swift -Onone %s -o %t/a.out
// RUN: %target-build-swift -O %s -o %t/a.out.optimized
// RUN: %target-codesign %t/a.out
// RUN: %target-codesign %t/a.out.optimized
//
// RUN: %target-run %t/a.out | %FileCheck --check-prefix CHECK %s
// RUN: %target-run %t/a.out.optimized | %FileCheck --check-prefix CHECK %s

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
func nongenericAnyIsPType(type: Any.Type) -> Bool {
  // `is P.Type` tests whether the argument conforms to `P`
  // Note:  this can only be true for a concrete type, never a protocol
  return type is P.Type
}
func nongenericAnyIsPProtocol(type: Any.Type) -> Bool {
  // `P.Protocol` is the metatype for `P` (the type of `P.self`)
  // `is P.Protocol` tests whether the argument is a subtype of `P`
  // In particular, it is true for `P.self`
  return type is P.Protocol
}
func nongenericAnyIsPAndAnyObjectType(type: Any.Type) -> Bool {
  return type is (P & AnyObject).Type
}
func nongenericAnyIsPAndAnyObjectProtocol(type: Any.Type) -> Bool {
  return type is (P & AnyObject).Protocol
}
func nongenericAnyIsPAndPCSubType(type: Any.Type) -> Bool {
  return type is (P & PCSub).Type
}
func genericAnyIs<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  // If we're testing against a runtime that doesn't have the fix this tests,
  // just pretend we got it right.
  if #available(SwiftStdlib 5.2, *) {
    // Remember: If `T` is bound to `P`, then `T.Type` is `P.Protocol`
    return type is T.Type
  } else {
    return expected
  }
}
// `as?` checks
func nongenericAnyAsConditionalPType(type: Any.Type) -> Bool {
  return (type as? P.Type) != nil
}
func nongenericAnyAsConditionalPProtocol(type: Any.Type) -> Bool {
  return (type as? P.Protocol) != nil
}
func nongenericAnyAsConditionalPAndAnyObjectType(type: Any.Type) -> Bool {
  return (type as? (P & AnyObject).Type) != nil
}
func nongenericAnyAsConditionalPAndAnyObjectProtocol(type: Any.Type) -> Bool {
  return (type as? (P & AnyObject).Protocol) != nil
}
func nongenericAnyAsConditionalPAndPCSubType(type: Any.Type) -> Bool {
  return (type as? (P & PCSub).Type) != nil
}
func genericAnyAsConditional<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  // If we're testing against a runtime that doesn't have the fix this tests,
  // just pretend we got it right.
  if #available(SwiftStdlib 5.3, *) {
    return (type as? T.Type) != nil
  } else {
    return expected
  }
}
// `as!` checks
func blackhole<T>(_ : T) { }

func nongenericAnyAsUnconditionalPType(type: Any.Type) -> Bool {
  blackhole(type as! P.Type)
  return true
}
func nongenericAnyAsUnconditionalPProtocol(type: Any.Type) -> Bool {
  blackhole(type as! P.Protocol)
  return true
}
func nongenericAnyAsUnconditionalPAndAnyObjectType(type: Any.Type) -> Bool {
  blackhole(type as! (P & AnyObject).Type)
  return true
}
func nongenericAnyAsUnconditionalPAndPCSubType(type: Any.Type) -> Bool {
  blackhole(type as! (P & PCSub).Type)
  return true
}
func genericAnyAsUnconditional<T>(type: Any.Type, to: T.Type, expected: Bool) -> Bool {
  if #available(SwiftStdlib 5.3, *) {
    blackhole(type as! T.Type)
  }
  return true
}

// CHECK-LABEL: casting types to protocols with generics:
print("casting types to protocols with generics:")
print(#line, nongenericAnyIsPType(type: P.self)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPProtocol(type: P.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyIs(type: P.self, to: P.self, expected: true)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyIsPType(type: PS.self)) // CHECK: [[@LINE]] true
print(#line, PS() is P) // CHECK: [[@LINE]] true
// One candidate for a Swift type theory holds that
// `A is a subtype of B iff A.self is metatype<B>`
// In that theory, `PS() is P` above would imply that
// `PS.self is P.Protocol` below must also be true.
// But that theory is not the one that Swift currently
// implements.
print(#line, nongenericAnyIsPProtocol(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PS.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPType(type: PE.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyIsPProtocol(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PE.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPType(type: PC.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyIsPProtocol(type: PC.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PC.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPType(type: PCSub.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyIsPProtocol(type: PCSub.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PCSub.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false

// CHECK-LABEL: conditionally casting types to protocols with generics:
print(#line, "conditionally casting types to protocols with generics:")
print(#line, nongenericAnyAsConditionalPType(type: P.self)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPProtocol(type: P.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsConditional(type: P.self, to: P.self, expected: true)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsConditionalPType(type: PS.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsConditionalPProtocol(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PS.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPType(type: PE.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsConditionalPProtocol(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PE.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPType(type: PC.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsConditionalPProtocol(type: PC.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PC.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPType(type: PCSub.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsConditionalPProtocol(type: PCSub.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PCSub.self, to: P.self, expected: false)) // CHECK: [[@LINE]] false

// CHECK-LABEL: unconditionally casting types to protocols with generics:
print(#line, "unconditionally casting types to protocols with generics:")
//print(#line, nongenericAnyAsUnconditionalPType(type: P.self)) // expected to trap
print(#line, nongenericAnyAsUnconditionalPProtocol(type: P.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsUnconditional(type: P.self, to: P.self, expected: true)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsUnconditionalPType(type: PS.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsUnconditional(type: PS.self, to: P.self, expected: true)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsUnconditionalPType(type: PE.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsUnconditional(type: PE.self, to: P.self, expected: true)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsUnconditionalPType(type: PC.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsUnconditional(type: PC.self, to: P.self, expected: true)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsUnconditionalPType(type: PCSub.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsUnconditional(type: PCSub.self, to: P.self, expected: true)) // CHECK: [[@LINE]] true

// CHECK-LABEL: casting types to protocol & AnyObject existentials:
print(#line, "casting types to protocol & AnyObject existentials:")
print(#line, nongenericAnyIsPAndAnyObjectType(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PS.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPAndAnyObjectType(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PE.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPAndAnyObjectType(type: PC.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyIsPAndAnyObjectProtocol(type: PC.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PC.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPAndAnyObjectType(type: PCSub.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyIsPAndAnyObjectProtocol(type: PCSub.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PCSub.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPAndAnyObjectType(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PS.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPAndAnyObjectType(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PE.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPAndAnyObjectType(type: PC.self)) // CHECK: [[@LINE]] true
print(#line, nongenericAnyAsConditionalPAndAnyObjectProtocol(type: PC.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PC.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPAndAnyObjectType(type: PCSub.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsConditional(type: PCSub.self, to: (P & AnyObject).self, expected: false)) // CHECK: [[@LINE]] false

// CHECK-LABEL: casting types to protocol & class existentials:
print(#line, "casting types to protocol & class existentials:")
print(#line, nongenericAnyIsPAndPCSubType(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PS.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPAndPCSubType(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyIs(type: PE.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false
// FIXME: reenable this when https://github.com/apple/swift/issues/53970 is fixed
//print(#line, nongenericAnyIsPAndPCSubType(type: PC.self)) // C HECK: [[@LINE]] false
print(#line, genericAnyIs(type: PC.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyIsPAndPCSubType(type: PCSub.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyIs(type: PCSub.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPAndPCSubType(type: PS.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PS.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPAndPCSubType(type: PE.self)) // CHECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PE.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false
// FIXME: reenable this when https://github.com/apple/swift/issues/53970 is fixed
// print(#line, nongenericAnyAsConditionalPAndPCSubType(type: PC.self)) // C HECK: [[@LINE]] false
print(#line, genericAnyAsConditional(type: PC.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false
print(#line, nongenericAnyAsConditionalPAndPCSubType(type: PCSub.self)) // CHECK: [[@LINE]] true
print(#line, genericAnyAsConditional(type: PCSub.self, to: (P & PCSub).self, expected: false)) // CHECK: [[@LINE]] false


// CHECK-LABEL: type comparisons:
print(#line, "type comparisons:\n")
print(#line, allMetasToAllMetas(Int.self, Int.self)) // CHECK: [[@LINE]] true
print(#line, allMetasToAllMetas(Int.self, Float.self)) // CHECK: [[@LINE]] false
print(#line, allMetasToAllMetas(C.self, C.self)) // CHECK: [[@LINE]] true
print(#line, allMetasToAllMetas(D.self, C.self)) // CHECK: [[@LINE]] true
print(#line, allMetasToAllMetas(C.self, D.self)) // CHECK: [[@LINE]] false
print(#line, C.self is D.Type) // CHECK: [[@LINE]] false
print(#line, (D.self as C.Type) is D.Type) // CHECK: [[@LINE]] true

let t: Any.Type = type(of: 1 as Any)
print(#line, t is Int.Type) // CHECK: [[@LINE]] true
print(#line, t is Float.Type) // CHECK: [[@LINE]] false
print(#line, t is C.Type) // CHECK: [[@LINE]] false

let u: Any.Type = type(of: (D() as Any))
print(#line, u is C.Type) // CHECK: [[@LINE]] true
print(#line, u is D.Type) // CHECK: [[@LINE]] true
print(#line, u is E.Type) // CHECK: [[@LINE]] false
print(#line, u is Int.Type) // CHECK: [[@LINE]] false

// FIXME: Can't spell AnyObject.Protocol
// CHECK-LABEL: AnyObject casts:
print(#line, "AnyObject casts:")
print(#line, allToAll(C(), AnyObject.self)) // CHECK: [[@LINE]] true

// On Darwin, the object will be the ObjC-runtime-class object;
// out of Darwin, this should not succeed.
print(#line, allToAll(type(of: C()), AnyObject.self))
// CHECK-objc: true
// CHECK-native: false

// Bridging
// NSNumber on Darwin, __SwiftValue on Linux.
print(#line, allToAll(0, AnyObject.self)) // CHECK: [[@LINE]] true

// This will get bridged using __SwiftValue.
struct NotBridged { var x: Int }
print(#line, allToAll(NotBridged(x: 0), AnyObject.self)) // CHECK: [[@LINE]] true

#if canImport(Foundation)
// This requires Foundation (for NSCopying):
print(#line, allToAll(NotBridged(x: 0), NSCopying.self)) // CHECK-objc: [[@LINE]] true
#endif

// On Darwin, these casts fail (intentionally) even though __SwiftValue does
// technically conform to these protocols through NSObject.
// Off Darwin, it should not conform at all.
print(#line, allToAll(NotBridged(x: 0), CustomStringConvertible.self)) // CHECK: [[@LINE]] false
print(#line, allToAll(NotBridged(x: 0), (AnyObject & CustomStringConvertible).self)) // CHECK: [[@LINE]] false

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
print(#line, "Bridge cast result: \(result)") // CHECK-NEXT-objc: Bridge cast result: true
#endif
