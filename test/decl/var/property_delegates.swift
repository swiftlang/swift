// RUN: %target-typecheck-verify-swift -swift-version 5

// ---------------------------------------------------------------------------
// Property delegate type definitions
// ---------------------------------------------------------------------------
@propertyDelegate
struct Wrapper<T> {
  var value: T
}

@propertyDelegate
struct WrapperWithInitialValue<T> {
  var value: T

  init(initialValue: T) {
    self.value = initialValue
  }
}

@propertyDelegate
struct WrapperAcceptingAutoclosure<T> {
  private let fn: () -> T

  var value: T {
    return fn()
  }

  init(initialValue fn: @autoclosure @escaping () -> T) {
    self.fn = fn
  }
}

@propertyDelegate
struct MissingValue<T> { }
// expected-error@-1{{property delegate type 'MissingValue' does not contain a non-static property named 'value'}}

@propertyDelegate
struct StaticValue {
  static var value: Int = 17
}
// expected-error@-3{{property delegate type 'StaticValue' does not contain a non-static property named 'value'}}


// expected-error@+1{{'@propertyDelegate' attribute cannot be applied to this declaration}}
@propertyDelegate
protocol CannotBeADelegate {
  associatedtype Value
  var value: Value { get set }
}

@propertyDelegate
struct NonVisibleValueDelegate<Value> {
  private var value: Value // expected-error{{private property 'value' cannot have more restrictive access than its enclosing property delegate type 'NonVisibleValueDelegate' (which is internal)}}
}

@propertyDelegate
struct NonVisibleInitDelegate<Value> {
  var value: Value

  private init(initialValue: Value) { // expected-error{{private initializer 'init(initialValue:)' cannot have more restrictive access than its enclosing property delegate type 'NonVisibleInitDelegate' (which is internal)}}
    self.value = initialValue
  }
}

@propertyDelegate
struct InitialValueTypeMismatch<Value> {
  var value: Value // expected-note{{'value' declared here}}

  init(initialValue: Value?) { // expected-error{{'init(initialValue:)' parameter type ('Value?') must be the same as its 'value' property type ('Value') or an @autoclosure thereof}}
    self.value = initialValue!
  }
}

@propertyDelegate
struct MultipleInitialValues<Value> { // expected-error{{property delegate type 'MultipleInitialValues' has multiple initial-value initializers}}
  var value: Value? = nil

  init(initialValue: Int) { // expected-note{{initializer 'init(initialValue:)' declared here}}
  }

  init(initialValue: Double) { // expected-note{{initializer 'init(initialValue:)' declared here}}
  }
}

// ---------------------------------------------------------------------------
// Property delegate type definitions
// ---------------------------------------------------------------------------
@propertyDelegate
struct _lowercaseDelegate<T> { // expected-error{{property delegate type name must start with an uppercase letter}}
  var value: T
}

@propertyDelegate
struct _UppercaseDelegate<T> {
  var value: T
}

// ---------------------------------------------------------------------------
// Limitations on where property delegates can be used
// ---------------------------------------------------------------------------

func testLocalContext() {
  @WrapperWithInitialValue // expected-error{{property delegates are not yet supported on local properties}}
  var x = 17
  x = 42
  _ = x
}

enum SomeEnum {
  case foo

  @Wrapper(value: 17)
  var bar: Int // expected-error{{property 'bar' declared inside an enum cannot have a delegate}}
  // expected-error@-1{{enums must not contain stored properties}}

  @Wrapper(value: 17)
  static var x: Int = 17 // okay
}

protocol SomeProtocol {
  @Wrapper(value: 17)
  var bar: Int // expected-error{{property 'bar' declared inside a protocol cannot have a delegate}}
  // expected-error@-1{{property in protocol must have explicit { get } or { get set } specifier}}

  @Wrapper(value: 17)
  static var x: Int // expected-error{{property 'x' declared inside a protocol cannot have a delegate}}
  // expected-error@-1{{property in protocol must have explicit { get } or { get set } specifier}}
}

struct HasDelegate { }

extension HasDelegate {
  @Wrapper(value: 17)
  var inExt: Int // expected-error{{property 'inExt' declared inside an extension cannot have a delegate}}
  // expected-error@-1{{extensions must not contain stored properties}}

  @Wrapper(value: 17)
  static var x: Int = 17 // okay
}

class ClassWithDelegates {
  @Wrapper(value: 17)
  var x: Int = 42

  
}

class Superclass {
  var x: Int = 0
}

class SubclassWithDelegate: Superclass {
  @Wrapper(value: 17)
  override var x: Int { get { return 0 } set { } } // expected-error{{property 'x' with attached delegate cannot override another property}}
}

class C { }

struct BadCombinations {
  @WrapperWithInitialValue
  lazy var x: C = C() // expected-error{{property 'x' with a delegate cannot also be lazy}}
  @Wrapper
  weak var y: C? // expected-error{{property 'y' with a delegate cannot also be weak}}
  @Wrapper
  unowned var z: C // expected-error{{property 'z' with a delegate cannot also be unowned}}
}

struct MultipleDelegates {
  @Wrapper(value: 17) // expected-error{{only one property delegate can be attached to a given property}}
  @WrapperWithInitialValue // expected-note{{previous property delegate specified here}}
  var x: Int = 17

  @WrapperWithInitialValue // expected-error 2{{property delegate can only apply to a single variable}}
  var (y, z) = (1, 2)
}
