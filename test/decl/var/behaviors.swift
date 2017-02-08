// RUN: %target-typecheck-verify-swift -enable-experimental-property-behaviors -module-name Main
// REQUIRES: property_behavior_value_substitution

protocol behavior {
  associatedtype Value
}
extension behavior {
  var value: Value { fatalError("") }
}

struct NotABehavior {}

var test1: Int __behavior behavior
var test2: String __behavior Main.behavior

// expected-error@+1{{property behavior name must refer to a protocol}}
var test5: String __behavior NotABehavior

struct Concrete {
  var test: String __behavior behavior
  static var test: String __behavior behavior
}

protocol Proto { }
extension Proto {
  var test: String __behavior behavior
  static var test: String __behavior behavior
}

struct Generic<T> {
  var test: String __behavior behavior
  static var test: String __behavior behavior
}

protocol SelfRequirement {}
protocol ValueRequirement {}

protocol constraints: SelfRequirement {
  associatedtype Value: ValueRequirement
}
extension constraints {
  var value: Value { fatalError("") }
}

struct VR: ValueRequirement {}
struct GVR<T>: ValueRequirement {}
struct NVR {}

// expected-error@+3{{property behavior 'constraints' can only be used on instance properties because it has 'Self' requirements}}
// expected-error@+2{{type 'NVR' does not conform to protocol 'ValueRequirement'}}
// expected-note@+1{{behavior 'constraints' requires property type to conform to protocol 'ValueRequirement'}}
var a: NVR __behavior constraints

// expected-error@+1{{property behavior 'constraints' can only be used on instance properties because it has 'Self' requirements}}
var b: VR __behavior constraints

struct SR: SelfRequirement {
  // expected-error@+2{{type 'NVR' does not conform to protocol 'ValueRequirement'}}
  // expected-note@+1{{behavior 'constraints' requires property type to conform to protocol 'ValueRequirement'}}
  var a: NVR __behavior constraints
  var b: VR __behavior constraints

  // expected-error@+3{{property behavior 'constraints' can only be used on instance properties because it has 'Self' requirements}}
  // expected-error@+2{{type 'NVR' does not conform to protocol 'ValueRequirement'}}
  // expected-note@+1{{behavior 'constraints' requires property type to conform to protocol 'ValueRequirement'}}
  static var a: NVR __behavior constraints
  // expected-error@+1{{property behavior 'constraints' can only be used on instance properties because it has 'Self' requirements}}
  static var b: VR __behavior constraints
}

// expected-error@+1 * {{type 'NSR' does not conform to protocol 'SelfRequirement'}}
struct NSR {
  // expected-note@+3{{conformance to 'SelfRequirement' required to contain an instance property with behavior 'constraints'}}
  // expected-error@+2{{type 'NVR' does not conform to protocol 'ValueRequirement'}}
  // expected-note@+1{{behavior 'constraints' requires property type to conform to protocol 'ValueRequirement'}}
  var a: NVR __behavior constraints
  // expected-note@+1{{conformance to 'SelfRequirement' required to contain an instance property with behavior 'constraints'}}
  var b: VR __behavior constraints
}

struct GSR<T>: SelfRequirement {
  var a: VR __behavior constraints
  var b: GVR<T> __behavior constraints
}

extension GSR where T: ValueRequirement {
  var c: T __behavior constraints
}

protocol nonBehaviorReqt {
  associatedtype Value

  func notBehaviorRelated() // expected-note{{declared here}}
}
extension nonBehaviorReqt {
  var value: Value { fatalError("") }
}

// expected-error@+1{{behavior protocol 'nonBehaviorReqt' has non-behavior requirement 'notBehaviorRelated'}}
var x: Int __behavior nonBehaviorReqt

protocol hasStorage {
  associatedtype Value

  var storage: Value? { get set }
}
extension hasStorage {
  static func initStorage() -> Value? {
    return nil
  }

  // Overloads that should be disregarded
  static func initStorage() -> Value {
    fatalError("")
  }
  static func initStorage(_: String) -> Value? {
    fatalError("")
  }

  var value: Value { fatalError("") }
}

var tuple = (0,0)

var storage1: Int __behavior hasStorage // expected-error {{not supported}}
struct Foo<T> {
  static var staticStorage1: T __behavior hasStorage // expected-error{{static stored properties not supported in generic types}}
  var storage2: T __behavior hasStorage

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = T

  func foo<U>(_: U) {
    var storage1: T __behavior hasStorage // expected-error {{not supported}}
    var storage2: U __behavior hasStorage // expected-error {{not supported}}

    _ = storage1
    _ = storage2
  }
}

struct Foo2<T> {
  var storage3: Int = 0 // expected-error {{initializer expression provided, but property behavior 'hasStorage' does not use it}}
    __behavior hasStorage

  var (storage4, storage5) = tuple // expected-error* {{initializer expression provided, but property behavior 'hasStorage' does not use it}}
    __behavior hasStorage

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = Int
}


extension Foo {
  static var y: T __behavior hasStorage // expected-error{{static stored properties not supported in generic types}}
  var y: T __behavior hasStorage // expected-error {{extensions may not contain stored properties}}
}

protocol storageWithoutInit {
  associatedtype Value

  var storage: Value { get set }
}
extension storageWithoutInit {
  var value: Value { fatalError("") }

  func initStorage() -> Int { fatalError("signature is wrong") } // expected-note * {{found this candidate}}
}

struct Bar {
  var x: Int __behavior storageWithoutInit // expected-error {{property behavior protocol has a 'storage' requirement but does not have a static 'initStorage' method with the expected type '() -> Self.Value'}}
}
class Bas {
  var x: Int __behavior storageWithoutInit // expected-error {{property behavior protocol has a 'storage' requirement but does not have a static 'initStorage' method with the expected type '() -> Self.Value'}}
}

protocol valueTypeMismatch {
  associatedtype Value
}
extension valueTypeMismatch {
  var value: Value? { fatalError("") } // expected-note {{'value' property declared here}}
}

var x: Int __behavior valueTypeMismatch // expected-error {{property behavior 'valueTypeMismatch' provides 'value' property implementation with type 'Int?' that doesn't match type 'x' of declared property 'Int'}}

protocol getset {
  associatedtype Value
}
extension getset {
  var value: Value { get { } set { } }
}

var testGetset: Int __behavior getset
class C<T> {
  var testGetset: T __behavior getset
  static var testGetset: T __behavior getset
}
struct S<T> {
  var testGetset: T __behavior getset
  static var testGetset: T __behavior getset
}

protocol parameterized {
  associatedtype Value
  func parameter() -> Value
}
extension parameterized {
  var value: Value {
    get { }
    set { }
  }
}

struct TestParameters {
  var hasParameter: Int __behavior parameterized { 0 }
  var (sharedParameter1, sharedParameter2): (Int,Int) 
    __behavior parameterized { 0 }  // expected-error {{multiple variables is not supported}} expected-error{{sharedParameter2}}
  var missingParameter: Int __behavior parameterized // expected-error{{requires a parameter}}
  var invalidParameter: Int __behavior parameterized { 5.5 } // expected-error{{cannot convert return expression of type 'Double' to return type 'Int'}}
}

// TODO
var globalParameter: Int __behavior parameterized { 0 } // expected-error{{not supported}}

protocol noParameter {
  associatedtype Value
}
extension noParameter {
  var value: Value {
    get { }
    set { }
  }
}

var hasNoParameter: Int __behavior noParameter
var hasUnwantedParameter: Int __behavior noParameter { 0 } // expected-error{{parameter expression provided, but property behavior 'noParameter' does not use it}}

protocol storageWithInitialValue {
  associatedtype Value
  var storage: Value? { get set }
}
extension storageWithInitialValue {
  var value: Value { get { } }

  static func initStorage(_ x: Value) -> Value? {
    return nil
  }
}

struct TestStorageWithInitialValue {
  var x: Int __behavior storageWithInitialValue // Would require DI
  var y = 0 __behavior storageWithInitialValue
  var z: Int = 5.5 __behavior storageWithInitialValue // expected-error {{cannot convert value of type 'Double' to type 'Int' in coercion}}
  var (a, b) = tuple __behavior storageWithInitialValue // expected-error* {{do not support destructuring}}

  // FIXME: Hack because we can't find the synthesized associated type witness
  // during witness matching.
  typealias Value = Int
}
