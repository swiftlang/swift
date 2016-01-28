// RUN: %target-parse-verify-swift -module-name Main

protocol behavior {
  associatedtype Value
}
extension behavior {
  var value: Value { fatalError("") }
}

struct NotABehavior {}

// FIXME: get/sets should not be required

var [behavior] test1: Int
var [Main.behavior] test2: String

// expected-error@+1{{expected behavior name}}
var [] test3: String
// expected-error@+1{{expected ']' after behavior name}}
var [behavior oops] test4: String

// expected-error@+1{{property behavior name must refer to a protocol}}
var [NotABehavior] test5: String

struct Concrete {
  var [behavior] test: String
  static var [behavior] test: String
}

protocol Proto { }
extension Proto {
  var [behavior] test: String
  static var [behavior] test: String
}

struct Generic<T> {
  var [behavior] test: String
  static var [behavior] test: String
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
var [constraints] a: NVR

// expected-error@+1{{property behavior 'constraints' can only be used on instance properties because it has 'Self' requirements}}
var [constraints] b: VR

struct SR: SelfRequirement {
  // expected-error@+2{{type 'NVR' does not conform to protocol 'ValueRequirement'}}
  // expected-note@+1{{behavior 'constraints' requires property type to conform to protocol 'ValueRequirement'}}
  var [constraints] a: NVR
  var [constraints] b: VR

  // expected-error@+3{{property behavior 'constraints' can only be used on instance properties because it has 'Self' requirements}}
  // expected-error@+2{{type 'NVR' does not conform to protocol 'ValueRequirement'}}
  // expected-note@+1{{behavior 'constraints' requires property type to conform to protocol 'ValueRequirement'}}
  static var [constraints] a: NVR
  // expected-error@+1{{property behavior 'constraints' can only be used on instance properties because it has 'Self' requirements}}
  static var [constraints] b: VR
}

// expected-error@+1 * {{type 'NSR' does not conform to protocol 'SelfRequirement'}}
struct NSR {
  // expected-note@+3{{conformance to 'SelfRequirement' required to contain an instance property with behavior 'constraints'}}
  // expected-error@+2{{type 'NVR' does not conform to protocol 'ValueRequirement'}}
  // expected-note@+1{{behavior 'constraints' requires property type to conform to protocol 'ValueRequirement'}}
  var [constraints] a: NVR
  // expected-note@+1{{conformance to 'SelfRequirement' required to contain an instance property with behavior 'constraints'}}
  var [constraints] b: VR
}

struct GSR<T>: SelfRequirement {
  var [constraints] a: VR
  var [constraints] b: GVR<T>
}

// FIXME: probably a phase order problem here that causes type checker to think
// T doesn't conform
extension GSR where T: ValueRequirement {
  // expected-error@+2{{type 'T' does not conform to protocol 'ValueRequirement'}}
  // expected-note@+1{{}}
  var [constraints] c: T
}

protocol nonBehaviorReqt {
  associatedtype Value

  func notBehaviorRelated() // expected-note{{declared here}}
}
extension nonBehaviorReqt {
  var value: Value { fatalError("") }
}

// expected-error@+1{{behavior protocol 'nonBehaviorReqt' has non-behavior requirement 'notBehaviorRelated'}}
var [nonBehaviorReqt] x: Int

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

var [hasStorage] storage1: Int // expected-error {{not supported}}
struct Foo<T> {
  static var [hasStorage] staticStorage1: T // expected-error{{static stored properties not supported in generic types}}
  var [hasStorage] storage2: T

  func foo<U>(_: U) {
    var [hasStorage] storage1: T // expected-error {{not supported}}
    var [hasStorage] storage2: U // expected-error {{not supported}}

    _ = storage1
    _ = storage2
  }
}
extension Foo {
  static var [hasStorage] y: T // expected-error{{static stored properties not supported in generic types}}
  var [hasStorage] y: T // expected-error {{extensions may not contain stored properties}}
}

protocol storageWithoutInit {
  associatedtype Value

  var storage: Value { get set }
}
extension storageWithoutInit {
  var value: Value { fatalError("") }

  func initStorage() -> Int { fatalError("signature is wrong") } // expected-note {{found this candidate}}
}

struct Bar {
  var [storageWithoutInit] x: Int // expected-error {{property behavior protocol has a 'storage' requirement but does not have a static 'initStorage' method with the expected type '() -> Self.Value'}}
}

protocol valueTypeMismatch {
  associatedtype Value
}
extension valueTypeMismatch {
  var value: Value? { fatalError("") } // expected-note {{'value' property declared here}}
}

var [valueTypeMismatch] x: Int // expected-error {{property behavior 'valueTypeMismatch' provides 'value' property implementation with type 'Int?' that doesn't match type 'x' of declared property 'Int'}}

protocol getset {
  associatedtype Value
}
extension getset {
  var value: Value { get { } set { } }
}

var [getset] testGetset: Int
class C<T> {
  var [getset] testGetset: T
  static var [getset] testGetset: T
}
struct S<T> {
  var [getset] testGetset: T
  static var [getset] testGetset: T
}
