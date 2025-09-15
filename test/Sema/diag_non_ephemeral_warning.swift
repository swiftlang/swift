// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -parse-as-library -o %t -module-name=ModuleA %S/Inputs/diag_non_ephemeral_module1.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -parse-as-library -o %t -module-name=ModuleB %S/Inputs/diag_non_ephemeral_module2.swift
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/main.swift %S/Inputs/diag_non_ephemeral_globals.swift

import ModuleA
import ModuleB

func takesMutableRaw(@_nonEphemeral _ x: UnsafeMutableRawPointer, _ y: Int) {}
func takesConst(@_nonEphemeral _ x: UnsafePointer<Int8>, _ y: Int) {}
func takesRaw(@_nonEphemeral _ x: UnsafeRawPointer) {}
func takesMutable(@_nonEphemeral _ x: UnsafeMutablePointer<Int8>) {}
func takesOptMutableRaw(@_nonEphemeral _ x: UnsafeMutableRawPointer?) {}
func takesOptConst(@_nonEphemeral _ x: UnsafePointer<Int8>?) {}

var str = ""
var optionalStr: String?

var arr: [Int8] = [5]
var optionalArr: [Int8]?

// We cannot use array-to-pointer and string-to-pointer conversions with
// non-ephemeral parameters.

takesMutableRaw(&arr, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use the 'withUnsafeMutableBytes' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesConst(str, 5) // expected-warning {{cannot pass 'String' to parameter; argument #1 must be a pointer that outlives the call to 'takesConst'}}
// expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'takesConst'}}
// expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}

takesConst(arr, 5) // expected-warning {{cannot pass '[Int8]' to parameter; argument #1 must be a pointer that outlives the call to 'takesConst'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'takesConst'}}
// expected-note@-2 {{use the 'withUnsafeBufferPointer' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&arr) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use the 'withUnsafeBytes' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutable(&arr) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutable'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call to 'takesMutable'}}
// expected-note@-2 {{use the 'withUnsafeMutableBufferPointer' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesOptMutableRaw(&arr) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesOptMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call to 'takesOptMutableRaw'}}
// expected-note@-2 {{use the 'withUnsafeMutableBytes' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

// FIXME: This currently uses inout-to-pointer instead of array-to-pointer
// (https://github.com/apple/swift/issues/51597).
takesOptMutableRaw(&optionalArr)

takesOptConst(arr) // expected-warning {{cannot pass '[Int8]' to parameter; argument #1 must be a pointer that outlives the call to 'takesOptConst'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call to 'takesOptConst'}}
// expected-note@-2 {{use the 'withUnsafeBufferPointer' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesOptConst(optionalArr) // expected-warning {{cannot pass '[Int8]?' to parameter; argument #1 must be a pointer that outlives the call to 'takesOptConst'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]?' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call to 'takesOptConst'}}

takesOptConst(str) // expected-warning {{cannot pass 'String' to parameter; argument #1 must be a pointer that outlives the call to 'takesOptConst'}}
// expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call to 'takesOptConst'}}
// expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}

takesOptConst(optionalStr) // expected-warning {{cannot pass 'String?' to parameter; argument #1 must be a pointer that outlives the call to 'takesOptConst'}}
// expected-note@-1 {{implicit argument conversion from 'String?' to 'UnsafePointer<Int8>?' produces a pointer valid only for the duration of the call to 'takesOptConst'}}

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

// We can also project stable pointer values from stored properties on such
// global and static stored variables, as long as the base type is a fragile
// struct or a tuple.
takesRaw(&C.staticStoredProperty.storedProperty)
takesRaw(&FinalC.staticStoredProperty.storedProperty)
takesRaw(&metatypeOfC.staticStoredProperty.storedProperty)
takesRaw(&type(of: topLevelC).staticStoredProperty.storedProperty)
takesRaw(&topLevelS.storedProperty)
takesRaw(&globalS.storedProperty)
takesRaw(&topLevelTupleOfS.0)
takesRaw(&topLevelTupleOfS.0.storedProperty)
takesRaw(&topLevelFragileS.storedProperty)

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
//   - Class bases

takesMutableRaw(&topLevelC.storedProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutableRaw(&topLevelFinalC.storedProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutableRaw(&topLevelFragileFinalC.storedProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutableRaw(&topLevelS.storedFinalC.storedProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

//   - Resilient global or static variables

takesMutableRaw(&globalResilient, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutableRaw(&ResilientStruct.staticStoredProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutableRaw(&type(of: topLevelResilientS).staticStoredProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

//   - Resilient struct or class bases

takesMutableRaw(&topLevelResilientS.storedProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutableRaw(&topLevelResilientFinalC.storedProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesMutableRaw(&topLevelOptOfResilientS!.storedProperty, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
// expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

//   - Protocol bases

takesRaw(&topLevelP.property) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&type(of: topLevelP).staticProperty) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelP[]) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

//   - Properties with observers

takesRaw(&topLevelWithObservers) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelS.storedPropertyWithObservers) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelOptOfS!.storedPropertyWithObservers) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelC.storedPropertyWithObservers) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelFinalC.storedPropertyWithObservers) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelTupleOfS.0.storedPropertyWithObservers) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

//   - Computed properties

takesRaw(&topLevelOptOfS!.computedProperty) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelC.computedProperty) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelFinalC.computedProperty) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelTupleOfS.0.computedProperty) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

//   - Subscripts

takesRaw(&topLevelS[]) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelC[]) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

takesRaw(&topLevelFinalC[]) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
// expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
// expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

//   - Local variables

func testInoutToPointerOfLocal() {
  var local: Int8 = 0

  takesMutableRaw(&local, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutableRaw'}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  takesRaw(&local) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
  // expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  takesMutable(&local) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesMutable'}}
  // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call to 'takesMutable'}}
  // expected-note@-2 {{use 'withUnsafeMutablePointer' in order to explicitly convert argument to pointer valid for a defined scope}}
}

//   - Instance members within types

struct S1 {
  var property: Int8 = 0
  mutating func foo() {
    takesRaw(&property) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
    // expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

    takesRaw(&self.property) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
    // expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}
  }
}

final class C1 {
  var property: Int8 = 0
  func foo() {
    takesRaw(&property) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
    // expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

    takesRaw(&self.property) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'takesRaw'}}
    // expected-note@-1 {{implicit argument conversion from 'Int8' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'takesRaw'}}
    // expected-note@-2 {{use 'withUnsafeBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}
  }
}

// Check that @_nonEphemeral is preserved through type inference.
let f1 = takesMutableRaw
f1(&arr, 5) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'f1'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'f1'}}
// expected-note@-2 {{use the 'withUnsafeMutableBytes' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

let f2 = takesConst
f2(arr, 5) // expected-warning {{cannot pass '[Int8]' to parameter; argument #1 must be a pointer that outlives the call to 'f2'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'f2'}}
// expected-note@-2 {{use the 'withUnsafeBufferPointer' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

let f3 = takesRaw
f3(&arr) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'f3'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeRawPointer' produces a pointer valid only for the duration of the call to 'f3'}}
// expected-note@-2 {{use the 'withUnsafeBytes' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

let f4 = takesMutable
f4(&arr) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'f4'}}
// expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafeMutablePointer<Int8>' produces a pointer valid only for the duration of the call to 'f4'}}
// expected-note@-2 {{use the 'withUnsafeMutableBufferPointer' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

struct S2 {
  static var selfProp = S2()
  static var selfPropWithObserver = S2() { didSet {} }

  static func takesConstStaticAndReturns<T>(@_nonEphemeral _ ptr: UnsafePointer<T>) -> S2 { return S2() }
  static func takesMutableRawStatic(_ x: String = "", @_nonEphemeral ptr: UnsafeMutableRawPointer) {}
  func takesMutableRaw(@_nonEphemeral ptr: UnsafeMutableRawPointer = UnsafeMutableRawPointer(&topLevelS)) {}

  subscript(@_nonEphemeral takesConstInt8 ptr: UnsafePointer<Int8>) -> Int {
    get { return 0 } set {}
  }
}

func testNonEphemeralInMembers() {
  var local = 0

  let _: S2 = .takesConstStaticAndReturns([1, 2, 3]) // expected-warning {{cannot pass '[Int]' to parameter; argument #1 must be a pointer that outlives the call to 'takesConstStaticAndReturns'}}
  // expected-note@-1 {{implicit argument conversion from '[Int]' to 'UnsafePointer<Int>' produces a pointer valid only for the duration of the call to 'takesConstStaticAndReturns'}}
  // expected-note@-2 {{use the 'withUnsafeBufferPointer' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  S2.takesMutableRawStatic(ptr: &local) // expected-warning {{cannot use inout expression here; argument 'ptr' must be a pointer that outlives the call to 'takesMutableRawStatic(_:ptr:)'}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRawStatic(_:ptr:)'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  S2.takesMutableRawStatic("", ptr: &local) // expected-warning {{cannot use inout expression here; argument 'ptr' must be a pointer that outlives the call to 'takesMutableRawStatic(_:ptr:)'}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRawStatic(_:ptr:)'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  var s2 = S2()
  s2.takesMutableRaw()
  s2.takesMutableRaw(ptr: &local) // expected-warning {{cannot use inout expression here; argument 'ptr' must be a pointer that outlives the call to 'takesMutableRaw(ptr:)'}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'takesMutableRaw(ptr:)'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  _ = s2[takesConstInt8: ""] // expected-warning {{cannot pass 'String' to parameter; argument 'takesConstInt8' must be a pointer that outlives the call to 'subscript(takesConstInt8:)'}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'subscript(takesConstInt8:)'}}
  // expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}

  s2[takesConstInt8: ""] += 1 // expected-warning {{cannot pass 'String' to parameter; argument 'takesConstInt8' must be a pointer that outlives the call to 'subscript(takesConstInt8:)'}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'subscript(takesConstInt8:)'}}
  // expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}

  _ = \S2.[takesConstInt8: ""] // expected-warning {{cannot pass 'String' to parameter; argument 'takesConstInt8' must be a pointer that outlives the call to 'subscript(takesConstInt8:)'}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'subscript(takesConstInt8:)'}}
  // expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}
}

func testNonEphemeralInDotMember() {
  func takesMutableS2(@_nonEphemeral _ ptr: UnsafeMutablePointer<S2>) {}
  takesMutableS2(&.selfProp)

  // FIXME: This should also produce a warning.
  takesMutableS2(&.selfPropWithObserver)
}

func testNonEphemeralWithVarOverloads() {
  takesRaw(&overloadedVar) // expected-error {{ambiguous use of 'overloadedVar'}}
  takesRaw(&overloadedVarOnlyOneResilient) // expected-error {{ambiguous use of 'overloadedVarOnlyOneResilient'}}
  takesRaw(&overloadedVarDifferentTypes) // expected-error {{ambiguous use of 'overloadedVarDifferentTypes'}}

  func takesIntPtr(@_nonEphemeral _ ptr: UnsafePointer<Int>) {}
  takesIntPtr(&overloadedVarDifferentTypes)

  func takesStringPtr(@_nonEphemeral _ ptr: UnsafePointer<String>) {}
  takesStringPtr(&overloadedVarDifferentTypes)
}

infix operator ^^^
func ^^^ (@_nonEphemeral lhs: UnsafeMutableRawPointer, rhs: Int) {}

func testNonEphemeralInOperators() {
  var local = 0

  &value ^^^ 1

  &local ^^^ 1 // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to '^^^'}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to '^^^'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}
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

  _ = S3(ptr1: &topLevelS, ptr2: &local) // expected-warning {{cannot use inout expression here; argument 'ptr2' must be a pointer that outlives the call to 'init(ptr1:ptr2:)'}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer?' produces a pointer valid only for the duration of the call to 'init(ptr1:ptr2:)'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  _ = S3.init(ptr1: &local, ptr2: &topLevelS) // expected-warning {{cannot use inout expression here; argument 'ptr1' must be a pointer that outlives the call to 'init(ptr1:ptr2:)'}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'init(ptr1:ptr2:)'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  _ = E.mutableRaw(&local) // expected-warning {{cannot use inout expression here; argument #1 must be a pointer that outlives the call to 'mutableRaw'}}
  // expected-note@-1 {{implicit argument conversion from 'Int' to 'UnsafeMutableRawPointer' produces a pointer valid only for the duration of the call to 'mutableRaw'}}
  // expected-note@-2 {{use 'withUnsafeMutableBytes' in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  _ = E.const([1, 2, 3]) // expected-warning {{cannot pass '[Int8]' to parameter; argument #1 must be a pointer that outlives the call to 'const'}}
  // expected-note@-1 {{implicit argument conversion from '[Int8]' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'const'}}
  // expected-note@-2 {{use the 'withUnsafeBufferPointer' method on Array in order to explicitly convert argument to buffer pointer valid for a defined scope}}

  _ = E.const("hello") // expected-warning {{cannot pass 'String' to parameter; argument #1 must be a pointer that outlives the call to 'const'}}
  // expected-note@-1 {{implicit argument conversion from 'String' to 'UnsafePointer<Int8>' produces a pointer valid only for the duration of the call to 'const'}}
  // expected-note@-2 {{use the 'withCString' method on String in order to explicitly convert argument to pointer valid for a defined scope}}
}

func ambiguous_fn(@_nonEphemeral _ ptr: UnsafePointer<Int>) {} // expected-note {{found this candidate}}
func ambiguous_fn(_ ptr: UnsafeRawPointer) {} // expected-note {{found this candidate}}

func test_ambiguity_with_function_instead_of_argument(_ x: inout Int) {
  ambiguous_fn(&x) // expected-error {{ambiguous use of 'ambiguous_fn'}}
}
