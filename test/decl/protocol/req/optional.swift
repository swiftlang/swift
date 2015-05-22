// RUN: %target-parse-verify-swift

// -----------------------------------------------------------------------
// Declaring optional requirements
// -----------------------------------------------------------------------
@objc class ObjCClass { }

@objc protocol P1 {
  optional func method(x: Int) // expected-note 2{{requirement 'method' declared here}}

  optional var prop: Int { get } // expected-note 2{{requirement 'prop' declared here}}

  optional subscript (i: Int) -> ObjCClass? { get } // expected-note 2{{requirement 'subscript' declared here}}
}

// -----------------------------------------------------------------------
// Providing witnesses for optional requirements
// -----------------------------------------------------------------------

// One does not have provide a witness for an optional requirement
class C1 : P1 { }

// ... but it's okay to do so.
class C2 : P1 {
  @objc func method(x: Int) { }

  @objc var prop: Int = 0

  @objc subscript (c: ObjCClass) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

// -----------------------------------------------------------------------
// "Near" matches.
// -----------------------------------------------------------------------

class C3 : P1 {
  func method(x: Int) { } 
  // expected-warning@-1{{non-@objc method 'method' cannot satisfy optional requirement of @objc protocol 'P1'}}{{3-3=@objc }}

  var prop: Int = 0
  // expected-warning@-1{{non-@objc property 'prop' cannot satisfy optional requirement of @objc protocol 'P1'}}{{3-3=@objc }}

  // expected-warning@+1{{non-@objc subscript cannot satisfy optional requirement of @objc protocol 'P1'}}{{3-3=@objc }}
  subscript (i: Int) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

class C4 { }

extension C4 : P1 {
  func method(x: Int) { } 
  // expected-warning@-1{{non-@objc method 'method' cannot satisfy optional requirement of @objc protocol 'P1'}}{{3-3=@objc }}

  var prop: Int { return 5 }
  // expected-warning@-1{{non-@objc property 'prop' cannot satisfy optional requirement of @objc protocol 'P1'}}{{3-3=@objc }}

  // expected-warning@+1{{non-@objc subscript cannot satisfy optional requirement of @objc protocol 'P1'}}{{3-3=@objc }}
  subscript (i: Int) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

class C5 : P1 { }

extension C5 {
  func method(x: Int) { } 

  var prop: Int { return 5 }

  subscript (i: Int) -> ObjCClass? {
    get {
      return nil
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
  _ = i as ObjCClass?
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
  _ = i as ObjCClass?
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
