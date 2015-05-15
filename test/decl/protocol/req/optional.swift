// RUN: %target-parse-verify-swift

// -----------------------------------------------------------------------
// Declaring optional requirements
// -----------------------------------------------------------------------
@objc protocol P1 {
  optional func method(x: Int)

  optional var prop: Int { get }

  optional subscript (i: Int) -> Int { get }
}

// -----------------------------------------------------------------------
// Providing witnesses for optional requirements
// -----------------------------------------------------------------------

// One does not have provide a witness for an optional requirement
class C1 : P1 { }

// ... but it's okay to do so.
class C2 : P1 {
  func method(x: Int) { }

  var prop: Int = 0

  subscript (i: Int) -> Int {
    get {
      return i
    }
    set {}
  }
}

// -----------------------------------------------------------------------
// Using optional requirements
// -----------------------------------------------------------------------

// Optional method references in generics.
func optionalMethodGeneric<T : P1>(t: T) {
  // Infers a value of optional type.
  var methodRef = t.method

  // Make sure it's an optional
  methodRef = .None

  // ... and that we can call it.
  methodRef!(5)
}

// Optional property references in generics.
func optionalPropertyGeneric<T : P1>(t: T) {
  // Infers a value of optional type.
  var propertyRef = t.prop

  // Make sure it's an optional
  propertyRef = .None

  // ... and that we can use it
  let i = propertyRef!
  _ = i as Int
}

// Optional subscript references in generics.
func optionalSubscriptGeneric<T : P1>(t: T) {
  // Infers a value of optional type.
  var subscriptRef = t[5]

  // Make sure it's an optional
  subscriptRef = .None

  // ... and that we can use it
  let i = subscriptRef!
  _ = i as Int
}

// Optional method references in existentials.
func optionalMethodExistential(t: P1) {
  // Infers a value of optional type.
  var methodRef = t.method

  // Make sure it's an optional
  methodRef = .None

  // ... and that we can call it.
  methodRef!(5)
}

// Optional property references in existentials.
func optionalPropertyExistential(t: P1) {
  // Infers a value of optional type.
  var propertyRef = t.prop

  // Make sure it's an optional
  propertyRef = .None

  // ... and that we can use it
  let i = propertyRef!
  _ = i as Int
}

// Optional subscript references in existentials.
func optionalSubscriptExistential(t: P1) {
  // Infers a value of optional type.
  var subscriptRef = t[5]

  // Make sure it's an optional
  subscriptRef = .None

  // ... and that we can use it
  let i = subscriptRef!
  _ = i as Int
}

// -----------------------------------------------------------------------
// Restrictions on the application of optional
// -----------------------------------------------------------------------

// optional cannot be used on non-protocol declarations
optional var optError: Int = 10 // expected-error{{'optional' can only be applied to protocol members}}

optional struct optErrorStruct { // expected-error{{'optional' modifier cannot be applied to this declaration}}
  optional var ivar: Int // expected-error{{'optional' can only be applied to protocol members}}
  optional func foo() { } // expected-error{{'optional' can only be applied to protocol members}}
}

optional class optErrorClass { // expected-error{{'optional' modifier cannot be applied to this declaration}}
  optional var ivar: Int = 0 // expected-error{{'optional' can only be applied to protocol members}}
  optional func foo() { } // expected-error{{'optional' can only be applied to protocol members}}
}
  
protocol optErrorProtocol {
  optional func foo(x: Int) // expected-error{{'optional' can only be applied to members of an @objc protocol}}
  optional typealias Assoc  // expected-error{{'optional' modifier cannot be applied to this declaration}}
}

@objc protocol optionalInitProto {
  optional init() // expected-error{{'optional' cannot be applied to an initializer}}
}
