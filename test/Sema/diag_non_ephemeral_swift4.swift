// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -parse-as-library -o %t -module-name=diag_non_ephemeral_module %S/Inputs/diag_non_ephemeral_module.swift
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -swift-version 4 -I %t %t/main.swift %S/Inputs/diag_non_ephemeral_globals.swift

import diag_non_ephemeral_module

func takesMutableRaw(_ x: @_nonEphemeral UnsafeMutableRawPointer, _ y: Int) {}
func takesConst(_ x: @_nonEphemeral UnsafePointer<Int8>, _ y: Int) {}
func takesRaw(_ x: @_nonEphemeral UnsafeRawPointer) {}
func takesMutable(_ x: @_nonEphemeral UnsafeMutablePointer<Int8>) {}
func takesOptMutableRaw(_ x: @_nonEphemeral UnsafeMutableRawPointer?) {}
func takesOptConst(_ x: @_nonEphemeral UnsafePointer<Int8>?) {}

var str = ""
var optionalStr: String?

var arr: [Int8] = [5]
var optionalArr: [Int8]?

// We cannot use array-to-pointer and string-to-pointer conversions with
// non-ephemeral parameters.

takesMutableRaw(&arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesConst(str, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

takesConst(arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

takesRaw(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesMutable(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}

takesOptMutableRaw(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer?' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}

// FIXME(SR-9100): This currently uses inout-to-pointer instead of array-to-pointer.
takesOptMutableRaw(&optionalArr)

takesOptConst(arr) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

takesOptConst(optionalArr) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]?' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

takesOptConst(str) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

takesOptConst(optionalStr) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'String?' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

struct S {
  static var staticStoredProperty: Int8 = 0
  var storedProperty: Int8 = 0
  var storedFinalC = FinalC()
  var storedPropertyWithObservers: Int8 = 0 { didSet {} }
  var computedProperty: Int8 { get { return 0 } set {} }
  subscript() -> Int8 { get { return 0 } set {} }
}

class C {
  static var staticStoredProperty = S()
  var storedProperty: Int8 = 0
  var storedPropertyWithObservers: Int8 = 0 { didSet {} }
  var computedProperty: Int8 { get { return 0 } set {} }
  subscript() -> Int8 { get { return 0 } set {} }
}

final class FinalC {
  static var staticStoredProperty = S()
  var storedProperty: Int8 = 0
  var storedPropertyWithObservers: Int8 = 0 { didSet {} }
  var computedProperty: Int8 { get { return 0 } set {} }
  subscript() -> Int8 { get { return 0 } set {} }
}

protocol P {
  static var staticProperty: Int8 { get set }
  var property: Int8 { get set }
  subscript() -> Int8 { get set }
}
func makeP() -> P { while true {} }

var value: Int8 = 5
var topLevelS = S()
var topLevelC = C()
var topLevelFinalC = FinalC()
var topLevelP = makeP()
var topLevelTupleOfS = (S(), S())
var topLevelOptOfS: S?
var topLevelWithObservers: Int8 = 0 { didSet {} }

var topLevelResilientS = ResilientStruct()
var topLevelOptOfResilientS: ResilientStruct?
var topLevelFragileS = FragileStruct()

var topLevelResilientFinalC = ResilientFinalClass()
var topLevelFragileFinalC = FragileFinalClass()

let metatypeOfC = C.self

// We can get stable pointer values from fragile global and static stored
// variables, as long as they don't have property observers.
takesMutableRaw(&value, 5)
takesMutableRaw(&str, 5)
takesMutableRaw(&topLevelS, 5)
takesRaw(&topLevelC)
takesRaw(&S.staticStoredProperty)
takesRaw(&C.staticStoredProperty)
takesRaw(&FinalC.staticStoredProperty)
takesRaw(&metatypeOfC.staticStoredProperty)
takesRaw(&type(of: topLevelC).staticStoredProperty)
takesRaw(&FragileStruct.staticStoredProperty)
takesRaw(&type(of: topLevelFragileS).staticStoredProperty)
takesRaw(&globalFragile)
takesRaw(&globalS)
takesRaw(&topLevelResilientS)
takesRaw(&topLevelFragileS)
takesRaw(&topLevelP)
takesRaw(&value)
takesRaw(&str)
takesMutable(&value)
takesOptMutableRaw(&value)
takesOptMutableRaw(&str)

extension C {
  static func foo() {
    takesRaw(&staticStoredProperty)
  }
}

// We can also project stable and, in the case of final classes, semi-stable pointer values
// from stored instance properties on such global and static stored variables, as long as
// the base type is a fragile struct, fragile final class, or a tuple.
takesRaw(&C.staticStoredProperty.storedProperty)
takesRaw(&FinalC.staticStoredProperty.storedProperty)
takesRaw(&metatypeOfC.staticStoredProperty.storedProperty)
takesRaw(&type(of: topLevelC).staticStoredProperty.storedProperty)
takesRaw(&topLevelFinalC.storedProperty)
takesRaw(&topLevelS.storedProperty)
takesRaw(&topLevelS.storedFinalC.storedProperty)
takesRaw(&globalS.storedProperty)
takesRaw(&topLevelTupleOfS.0)
takesRaw(&topLevelTupleOfS.0.storedProperty)
takesRaw(&topLevelFragileS.storedProperty)
takesRaw(&topLevelFragileFinalC.storedProperty)

extension C {
  static func bar() {
    takesRaw(&staticStoredProperty.storedProperty)
  }
}

// We can also project through force unwraps.
takesRaw(&topLevelOptOfS!)
takesRaw(&topLevelOptOfS!.storedProperty)
takesRaw(&topLevelOptOfResilientS!)

// But we cannot do the same for:
//   - Non-final class bases

takesMutableRaw(&topLevelC.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

//   - Resilient global or static variables

takesMutableRaw(&globalResilient, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesMutableRaw(&ResilientStruct.staticStoredProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesMutableRaw(&type(of: topLevelResilientS).staticStoredProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

//   - Resilient struct or class bases

takesMutableRaw(&topLevelResilientS.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesMutableRaw(&topLevelResilientFinalC.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesMutableRaw(&topLevelOptOfResilientS!.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

//   - Protocol bases

takesRaw(&topLevelP.property) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&type(of: topLevelP).staticProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelP[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

//   - Properties with observers

takesRaw(&topLevelWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelS.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelOptOfS!.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelC.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelFinalC.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelTupleOfS.0.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

//   - Computed properties

takesRaw(&topLevelOptOfS!.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelC.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelFinalC.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelTupleOfS.0.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

//   - Subscripts

takesRaw(&topLevelS[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelC[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&topLevelFinalC[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

//   - Local variables

func testInoutToPointerOfLocal() {
  var local: Int8 = 0

  takesMutableRaw(&local, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  takesRaw(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  takesMutable(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}
}

//   - Instance members within types

struct S1 {
  var property: Int8 = 0
  mutating func foo() {
    takesRaw(&property) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

    takesRaw(&self.property) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}
  }
}

final class C1 {
  var property: Int8 = 0
  func foo() {
    takesRaw(&property) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

    takesRaw(&self.property) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}
  }
}

// Check that @_non_ephemeral is preserved through type inference.
let f1 = takesMutableRaw
f1(&arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

let f2 = takesConst
f2(arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

let f3 = takesRaw
f3(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

let f4 = takesMutable
f4(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}

struct S2 {
  static func takesConstStaticAndReturns<T>(_ ptr: @_nonEphemeral UnsafePointer<T>) -> S2 { return S2() }
  static func takesMutableRawStatic(_ x: String = "", ptr: @_nonEphemeral UnsafeMutableRawPointer) {}
  func takesMutableRaw(ptr: @_nonEphemeral UnsafeMutableRawPointer = UnsafeMutableRawPointer(&topLevelS)) {}

  subscript(takesConstInt8 ptr: @_nonEphemeral UnsafePointer<Int8>) -> Int {
    get { return 0 } set {}
  }
}

func testNonEphemeralInMembers() {
  var local = 0

  let _: S2 = .takesConstStaticAndReturns([1, 2, 3]) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call}}

  S2.takesMutableRawStatic(ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  S2.takesMutableRawStatic("", ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  var s2 = S2()
  s2.takesMutableRaw() // okay.
  s2.takesMutableRaw(ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = s2[takesConstInt8: ""] // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  s2[takesConstInt8: ""] += 1 // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}
}

infix operator ^^^
func ^^^ (lhs: @_nonEphemeral UnsafeMutableRawPointer, rhs: Int) {}

func testNonEphemeralInOperators() {
  var local = 0

  &value ^^^ 1

  &local ^^^ 1 // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

func testNonEphemeralInClosures() {
  var local = 0

  let fn: (@_nonEphemeral UnsafeMutableRawPointer) -> Void = { _ in }
  fn(&value)
  fn(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

struct S3 {
  var ptr1: UnsafeMutableRawPointer
  lazy var ptr2 = UnsafeMutableRawPointer(&topLevelS)
}

enum E {
  case mutableRaw(UnsafeMutableRawPointer)
  case const(UnsafePointer<Int8>)
}

func testNonEphemeralInMemberwiseInits() {
  var local = 0

  _ = S3(ptr1: &topLevelS, ptr2: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer?' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = S3(ptr1: &local, ptr2: &topLevelS) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = E.mutableRaw(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  _ = E.const([1, 2, 3]) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

  _ = E.const("hello") // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}
}
