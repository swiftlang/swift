// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -o %t -module-name=diag_non_ephemeral_module %S/Inputs/diag_non_ephemeral_module.swift
// RUN: %target-typecheck-verify-swift -swift-version 4 -I %t

import diag_non_ephemeral_module

var str = ""
var optionalStr: String?

var arr: [Int8] = [5]
var optionalArr: [Int8]?

var value: Int8 = 5

func takesMutableRaw(_ x: @_nonEphemeral UnsafeMutableRawPointer, _ y: Int) {}
func takesConst(_ x: @_nonEphemeral UnsafePointer<Int8>, _ y: Int) {}
func takesRaw(_ x: @_nonEphemeral UnsafeRawPointer) {}
func takesMutable(_ x: @_nonEphemeral UnsafeMutablePointer<Int8>) {}
func takesOptMutableRaw(_ x: @_nonEphemeral UnsafeMutableRawPointer?) {}
func takesOptConst(_ x: @_nonEphemeral UnsafePointer<Int8>?) {}

takesMutableRaw(&arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesConst(str, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

takesConst(arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

takesRaw(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesMutable(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}

takesOptMutableRaw(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer?' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}

// FIXME(SR-9100): This currently uses inout-to-pointer instead of array-to-pointer.
takesOptMutableRaw(&optionalArr)

takesOptConst(arr) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

takesOptConst(optionalArr) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]?' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

takesOptConst(str) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

takesOptConst(optionalStr) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>?' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'String?' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call}}

// These are all okay, `value` and `str` are top-level variables and therefore get static pointer values when used with inout.
takesMutableRaw(&value, 5)
takesMutableRaw(&str, 5)
takesRaw(&value)
takesRaw(&str)
takesMutable(&value)
takesOptMutableRaw(&value)
takesOptMutableRaw(&str)

struct S {
  var storedProperty: Int8 = 0
  var storedPropertyWithObservers: Int8 = 0 { didSet {} }
  var computedProperty: Int8 { get { return 0 } set {} }
  subscript() -> Int8 { get { return 0 } set {} }
}

class C {
  var storedProperty: Int8 = 0
  var storedPropertyWithObservers: Int8 = 0 { didSet {} }
  var computedProperty: Int8 { get { return 0 } set {} }
  subscript() -> Int8 { get { return 0 } set {} }
}

var globalC = C()
var globalS = S()
var globalTupleOfS = (S(), S())
var globalOptOfS: S?
var globalWithObservers: Int8 = 0 { didSet {} }

var globalResilient = ResilientStruct()
var globalOptOfResilient: ResilientStruct?
var globalFragile = FragileStruct()

// These are all okay, we can get stable pointer values through force unwraps and stored field accesses on global fragile structure and tuple variables.
takesMutableRaw(&globalS, 5)
takesRaw(&globalC)
takesRaw(&globalS.storedProperty)
takesRaw(&globalTupleOfS.0)
takesRaw(&globalTupleOfS.0.storedProperty)
takesRaw(&globalOptOfS!)
takesRaw(&globalOptOfS!.storedProperty)
takesRaw(&globalResilient)
takesRaw(&globalOptOfResilient!)
takesRaw(&globalFragile)
takesRaw(&globalFragile.storedProperty)

// But we cannot do the same for class or resilient struct bases, properties with observers, computed properties or subscripts.

takesMutableRaw(&globalResilient.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesMutableRaw(&globalOptOfResilient!.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesMutableRaw(&globalC.storedProperty, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalC.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalC.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalC[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalS[]) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalS.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalTupleOfS.0.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalTupleOfS.0.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalOptOfS!.computedProperty) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalOptOfS!.storedPropertyWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

takesRaw(&globalWithObservers) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

// The following are not okay as they're local.
func testInoutToPointerOfLocal() {
  var local: Int8 = 0

  takesMutableRaw(&local, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  takesRaw(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

  takesMutable(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}
}

// Check that @_non_ephemeral is preserved through type inference.
let f1 = takesMutableRaw
f1(&arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

let f2 = takesConst
f2(arr, 5) // expected-warning {{passing temporary pointer argument of type 'UnsafePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call}}

let f3 = takesRaw
f3(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call}}

let f4 = takesMutable
f4(&arr) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutablePointer<Int8>' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call}}

struct S1 {
  static func takesMutableRaw(_ x: String = "", ptr: @_nonEphemeral UnsafeMutableRawPointer) {}
  func takesConst(ptr: @_nonEphemeral UnsafeMutableRawPointer = UnsafeMutableRawPointer(&globalS)) {}
}

func testNonEphemeralInMethods() {
  var local = 0

  S1.takesMutableRaw(ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  S1.takesMutableRaw("", ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}

  let s1 = S1()
  s1.takesConst() // okay.
  s1.takesConst(ptr: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

infix operator ^^^
func ^^^ (lhs: @_nonEphemeral UnsafeMutableRawPointer, rhs: Int) {}

func testNonEphemeralInOperators() {
  var local = 0

  &local ^^^ 1 // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

func testNonEphemeralInClosures() {
  var local = 0

  let fn: (@_nonEphemeral UnsafeMutableRawPointer) -> Void = { _ in }
  fn(&local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}

struct S2 {
  var ptr1: UnsafeMutableRawPointer
  lazy var ptr2 = UnsafeMutableRawPointer(&globalS)
}

func testNonEphemeralInMemberwiseInit() {
  var local = 0

  _ = S2(ptr1: &globalS, ptr2: &local) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer?' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call}}

  _ = S2(ptr1: &local, ptr2: &globalS) // expected-warning {{passing temporary pointer argument of type 'UnsafeMutableRawPointer' to parameter expecting a pointer that outlives the duration of the call leads to undefined behaviour; this will be an error in a future release}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call}}
}
