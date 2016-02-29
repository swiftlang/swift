// RUN: %target-parse-verify-swift -enable-experimental-property-behaviors -module-name Main

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
  static func initStorage(_: Int) -> Value? {
    fatalError("")
  }
  static func initStorage(_: Value) -> Value? {
    fatalError("")
  }

  var value: Value { fatalError("") }
}

var storage1: Int __behavior hasStorage // expected-error {{not supported}}
struct Foo<T> {
  static var staticStorage1: T __behavior hasStorage // expected-error{{static stored properties not supported in generic types}}
  var storage2: T __behavior hasStorage

  func foo<U>(_: U) {
    var storage1: T __behavior hasStorage // expected-error {{not supported}}
    var storage2: U __behavior hasStorage // expected-error {{not supported}}

    _ = storage1
    _ = storage2
  }
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

protocol initialValue {
  associatedtype Value
  static var initialValue: Value { get }
}
extension initialValue {
  var value: Value {
    get { }
    set { }
  }
}

let compoundInitialValue = (0,0)

struct TestInitialValues {
  var hasInitialValue: Int = 0 __behavior initialValue
  var (sharedInitialValue1, sharedInitialValue2): (Int,Int) 
    = compoundInitialValue __behavior initialValue // expected-error * {{cannot destructure}}
  var missingInitialValue: Int __behavior initialValue // expected-error{{requires an initializer}}
  // TODO: "return expression" message is wrong
  var invalidInitialValue: Int = 5.5 __behavior initialValue // expected-error{{cannot convert return expression of type 'Double' to return type 'Int'}}
}

// TODO
var globalInitialValue: Int = 0 __behavior initialValue // expected-error{{not supported}}

protocol noInitialValue {
  associatedtype Value
}
extension noInitialValue {
  var value: Value {
    get { }
    set { }
  }
}

var hasNoInitialValue: Int __behavior noInitialValue
var hasUnwantedInitialValue: Int = 0 __behavior noInitialValue // expected-error{{initializer expression provided, but property behavior 'noInitialValue' does not use it}}
var (hasUnwantedSharedInitialValue1,
     hasUnwantedSharedInitialValue2): (Int, Int)
  = compoundInitialValue // expected-error * {{initializer expression provided, but property behavior 'noInitialValue' does not use it}}
  __behavior noInitialValue
