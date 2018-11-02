// RUN: %target-typecheck-verify-swift -enable-objc-interop

// -----------------------------------------------------------------------
// Declaring optional requirements
// -----------------------------------------------------------------------
@objc class ObjCClass { }

@objc protocol P1 {
  @objc optional func method(_ x: Int) // expected-note {{requirement 'method' declared here}}

  @objc optional var prop: Int { get }

  @objc optional subscript (i: Int) -> ObjCClass? { get }
}

@objc protocol P2 {
  @objc(objcMethodWithInt:)
  optional func method(y: Int) // expected-note 1{{requirement 'method(y:)' declared here}}
}

// -----------------------------------------------------------------------
// Providing witnesses for optional requirements
// -----------------------------------------------------------------------

// One does not have provide a witness for an optional requirement
class C1 : P1 { }

// ... but it's okay to do so.
class C2 : P1 {
  @objc func method(_ x: Int) { }

  @objc var prop: Int = 0

  @objc subscript (i: Int) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

class C3 : P1 {
  func method(_ x: Int) { } 

  var prop: Int = 0

  subscript (i: Int) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

class C4 { }

extension C4 : P1 {
  func method(_ x: Int) { } 

  var prop: Int { return 5 }

  subscript (i: Int) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

// -----------------------------------------------------------------------
// Okay to match via extensions.
// -----------------------------------------------------------------------

class C5 : P1 { }

extension C5 {
  func method(_ x: Int) { } 

  var prop: Int { return 5 }

  subscript (i: Int) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

// Note: @nonobjc suppresses witness match.
class C6 { }

extension C6 : P1 {
  @nonobjc func method(_ x: Int) { } 

  @nonobjc var prop: Int { return 5 }

  @nonobjc subscript (i: Int) -> ObjCClass? {
    get {
      return nil
    }
    set {}
  }
}

// -----------------------------------------------------------------------
// "Near" matches.
// -----------------------------------------------------------------------

// Note: warn about selector matches where the Swift names didn't match.
@objc class C7 : P1 { // expected-note{{class 'C7' declares conformance to protocol 'P1' here}}
  @objc(method:) func otherMethod(x: Int) { } // expected-error{{Objective-C method 'method:' provided by method 'otherMethod(x:)' conflicts with optional requirement method 'method' in protocol 'P1'}}
  // expected-note@-1{{rename method to match requirement 'method'}}{{23-34=method}}{{35-35=_ }}
}

@objc class C8 : P2 { // expected-note{{class 'C8' declares conformance to protocol 'P2' here}}
  @objc func objcMethod(int x: Int) { } // expected-error{{Objective-C method 'objcMethodWithInt:' provided by method 'objcMethod(int:)' conflicts with optional requirement method 'method(y:)' in protocol 'P2'}}
  // expected-note@-1{{rename method to match requirement 'method(y:)'}}{{14-24=method}}{{25-28=y}}{{none}}
}


// -----------------------------------------------------------------------
// Using optional requirements
// -----------------------------------------------------------------------

// Optional method references in generics.
func optionalMethodGeneric<T : P1>(_ t: T) {
  // Infers a value of optional type.
  var methodRef = t.method

  // Make sure it's an optional
  methodRef = .none

  // ... and that we can call it.
  methodRef!(5)
}

// Optional property references in generics.
func optionalPropertyGeneric<T : P1>(_ t: T) {
  // Infers a value of optional type.
  var propertyRef = t.prop

  // Make sure it's an optional
  propertyRef = .none

  // ... and that we can use it
  let i = propertyRef!
  _ = i as Int
}

// Optional subscript references in generics.
func optionalSubscriptGeneric<T : P1>(_ t: T) {
  // Infers a value of optional type.
  var subscriptRef = t[5]

  // Make sure it's an optional
  subscriptRef = .none

  // ... and that we can use it
  let i = subscriptRef!
  _ = i as ObjCClass?
}

// Optional method references in existentials.
func optionalMethodExistential(_ t: P1) {
  // Infers a value of optional type.
  var methodRef = t.method

  // Make sure it's an optional
  methodRef = .none

  // ... and that we can call it.
  methodRef!(5)
}

// Optional property references in existentials.
func optionalPropertyExistential(_ t: P1) {
  // Infers a value of optional type.
  var propertyRef = t.prop

  // Make sure it's an optional
  propertyRef = .none

  // ... and that we can use it
  let i = propertyRef!
  _ = i as Int
}

// Optional subscript references in existentials.
func optionalSubscriptExistential(_ t: P1) {
  // Infers a value of optional type.
  var subscriptRef = t[5]

  // Make sure it's an optional
  subscriptRef = .none

  // ... and that we can use it
  let i = subscriptRef!
  _ = i as ObjCClass?
}

// -----------------------------------------------------------------------
// Restrictions on the application of optional
// -----------------------------------------------------------------------

// optional cannot be used on non-protocol declarations
optional var optError: Int = 10 // expected-error{{'optional' can only be applied to protocol members}} {{1-10=}}

optional struct optErrorStruct { // expected-error{{'optional' modifier cannot be applied to this declaration}} {{1-10=}}
  optional var ivar: Int // expected-error{{'optional' can only be applied to protocol members}} {{3-12=}}
  optional func foo() { } // expected-error{{'optional' can only be applied to protocol members}} {{3-12=}}
}

optional class optErrorClass { // expected-error{{'optional' modifier cannot be applied to this declaration}} {{1-10=}}
  optional var ivar: Int = 0 // expected-error{{'optional' can only be applied to protocol members}} {{3-12=}}
  optional func foo() { } // expected-error{{'optional' can only be applied to protocol members}} {{3-12=}}
}

protocol optErrorProtocol {
  optional func foo(_ x: Int) // expected-error{{'optional' can only be applied to members of an @objc protocol}}
}

@objc protocol optObjcAttributeProtocol {
  optional func foo(_ x: Int) // expected-error{{'optional' requirements are an Objective-C compatibility feature; add '@objc'}}{{3-3=@objc }}
  optional var bar: Int { get } // expected-error{{'optional' requirements are an Objective-C compatibility feature; add '@objc'}}{{3-3=@objc }}
  optional associatedtype Assoc  // expected-error{{'optional' modifier cannot be applied to this declaration}} {{3-12=}}
  // expected-error@-1 {{associated type 'Assoc' cannot be declared inside '@objc' protocol 'optObjcAttributeProtocol'}}
}

@objc protocol optionalInitProto {
  @objc optional init() // expected-error{{'optional' cannot be applied to an initializer}}
}
