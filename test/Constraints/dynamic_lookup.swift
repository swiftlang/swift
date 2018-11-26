// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/PrivateObjC.swift -o %t
// RUN: %target-typecheck-verify-swift -I %t -verify-ignore-unknown

// REQUIRES: objc_interop
import Foundation
import PrivateObjC

@objc
class A {
  init() {}
}
@objc
class B : A {
  override init() { super.init() }
}
@objc
class C {
  init() {}
}

class X {
  init() {} 

  @objc func foo(_ i: Int) { }
  @objc func bar() { }

  @objc func ovl2() -> A { } // expected-note{{found this candidate}}

  @objc func ovl4() -> B { }
  @objc func ovl5() -> B { } // expected-note{{found this candidate}}

  @objc class func staticFoo(_ i : Int) { }

  @objc func prop3() -> Int { return 5 }
}

class Y : P {
  init() {} 

  @objc func foo(_ s: String) { }
  @objc func wibble() { } // expected-note 2 {{did you mean 'wibble'?}}

  @objc func ovl1() -> A { }

  @objc func ovl4() -> B { }
  @objc func ovl5() -> C { } // expected-note{{found this candidate}}

  @objc var prop1 : Int {
    get {
      return 5
    }
  }

  var _prop2 : String

  @objc var prop2 : String {
    get {
      return _prop2
    }
    set(value) {
      _prop2 = value
    }
  }

  @objc var prop3 : Int {
    get {
      return 5
    }
  }

  @objc subscript (idx : Int) -> String {
    get {
      return "hello"
    }
    set {}
  }
}

class Z : Y {
  @objc override func ovl1() -> B { }
  @objc func ovl2() -> C { } // expected-note{{found this candidate}}
  @objc(ovl3_A) func ovl3() -> A { }
  @objc func ovl3() -> B { }
  func generic4<T>(_ x : T) { }
}

@objc protocol P {
  func wibble()
}

@objc protocol P2 {
  func wonka()
  var protoProp : Int { get }
  static func staticWibble()

  subscript (idx : A) -> Int { get set }
}

struct S {
  func wobble() { }
}

class D<T> {
  func generic1(_ x : T) { }
}

extension Z {
  @objc func ext1() -> A { }
}

// Find methods via dynamic method lookup.
typealias Id = AnyObject
var obj : Id = X()
obj.bar!()
obj.foo!(5)
obj.foo!("hello")
obj.wibble!()
obj.wobble!() // expected-error{{value of type 'Id' (aka 'AnyObject') has no member 'wobble'}}
obj.ext1!()  // expected-warning {{result of call to function returning 'A' is unused}}
obj.wonka!()

// Same as above but without the '!'
obj.bar()
obj.foo(5)
obj.foo("hello")
obj.wibble()
obj.wobble() // expected-error{{value of type 'Id' (aka 'AnyObject') has no member 'wobble'}}
obj.ext1()  // expected-warning {{result of call to function returning 'A' is unused}}
obj.wonka()

// Find class methods via dynamic method lookup.
type(of: obj).staticFoo!(5)
type(of: obj).staticWibble!()

// Same as above but without the '!'
type(of: obj).staticFoo(5)
type(of: obj).staticWibble()

// Overloading and ambiguity resolution

// When we have overriding, pick the least restrictive declaration.
var ovl1Result = obj.ovl1!()
ovl1Result = A() // verify that we got an A, not a B

// Same as above but without the '!'
obj.ovl1()  // expected-warning {{result of call to function returning 'A' is unused}}

// Don't allow overload resolution between declarations from different
// classes.
var ovl2ResultA = obj.ovl2!() // expected-error{{ambiguous use of 'ovl2()'}}

// ... but it's okay to allow overload resolution between declarations
// from the same class.
var ovl3Result = obj.ovl3!()
ovl3Result = B()

// For [objc] declarations, we can ignore declarations with the same
// selector and type.
var ovl4Result = obj.ovl4!()

// ... but not when the types are different.
var ovl5Result = obj.ovl5!() // expected-error{{ambiguous use of 'ovl5()'}}

// Same as above but without the '!'
obj.ovl4()  // expected-warning {{result of call to function returning 'B' is unused}}

// Generics

// Dynamic lookup cannot find members of a generic class (or a class
// that's a member of anything generic), because we wouldn't be able
// to figure out the generic arguments.
var generic1Result = obj.generic1!(17) // expected-error{{value of type 'Id' (aka 'AnyObject') has no member 'generic1'}}
obj.generic2!() // expected-error{{value of type 'Id' (aka 'AnyObject') has no member 'generic2'}}
obj.generic3!() // expected-error{{value of type 'Id' (aka 'AnyObject') has no member 'generic3'}}

// Dynamic method lookup can't find non-[objc] members
obj.generic4!(5) // expected-error{{value of type 'Id' (aka 'AnyObject') has no member 'generic4'}}

// Find properties via dynamic lookup.
var prop1Result : Int = obj.prop1!
var prop2Result : String = obj.prop2!
obj.prop2 = "hello" // expected-error{{cannot assign to immutable expression of type 'String?'}}
var protoPropResult : Int = obj.protoProp!

// Find subscripts via dynamic lookup
var sub1Result : String = obj[5]!
var sub2Result : Int = obj[A()]!

// Subscript then call without the '!'
var sub1ResultNE = obj[5].hasPrefix("foo")
var sub2ResultNE = obj[A()].hashValue

// Property/function ambiguities.
var prop3ResultA : Int? = obj.prop3
var prop3ResultB : (() -> Int)? = obj.prop3
var prop3ResultC = obj.prop3
let prop3ResultCChecked: Int? = prop3ResultC

var obj2 : AnyObject & P = Y()

class Z2 { }
class Z3<T : AnyObject> { }
class Z4<T> where T : AnyObject { }

// Don't allow one to call instance methods on the Type via
// dynamic method lookup.
type(of: obj).foo!(obj)(5) // expected-error{{instance member 'foo' cannot be used on type 'Id' (aka 'AnyObject')}}

// Checked casts to AnyObject
var p: P = Y()
// expected-warning @+1 {{forced cast from 'P' to 'AnyObject' always succeeds; did you mean to use 'as'?}}
var obj3 : AnyObject = (p as! AnyObject)! // expected-error{{cannot force unwrap value of non-optional type 'AnyObject'}} {{41-42=}}

// Implicit force of an implicitly unwrapped optional
let uopt : AnyObject! = nil
uopt.wibble!()

// Should not be able to see private or internal @objc methods.
uopt.privateFoo!() // expected-error{{value of type 'AnyObject?' has no member 'privateFoo'}}
uopt.internalFoo!() // expected-error{{value of type 'AnyObject?' has no member 'internalFoo'}}

let anyValue: Any = X()
_ = anyValue.bar() // expected-error {{value of type 'Any' has no member 'bar'}}
// expected-note@-1 {{cast 'Any' to 'AnyObject' or use 'as!' to force downcast to a more specific type to access members}}{{5-5=(}}{{13-13= as AnyObject)}}
_ = (anyValue as AnyObject).bar()
_ = (anyValue as! X).bar()

var anyDict: [String : Any] = Dictionary<String, Any>()
anyDict["test"] = anyValue
_ = anyDict["test"]!.bar() // expected-error {{value of type 'Any' has no member 'bar'}}
// expected-note@-1 {{cast 'Any' to 'AnyObject' or use 'as!' to force downcast to a more specific type to access members}}{{5-5=(}}{{21-21= as AnyObject)}}

// Test that overload resolution during constraint solving of values
// looked-up dynamically through AnyObject are treated as conforming
// to the protocols they are supposed to conform to.
class NSObjDerived1 : NSObject {
  @objc var everything: [Any] = []
}

class NSObjDerived2 : NSObject {
  var everything: Any = 1
}

func rdar29960565(_ o: AnyObject) {
  for i in o.everything {
    _ = i
  }
}

// FIXME: Remove -verify-ignore-unknown.
// <unknown>:0: error: unexpected note produced: 'privateFoo' declared here
// <unknown>:0: error: unexpected note produced: 'internalFoo' declared here

@objc protocol Q {}

@objc class Dynamic : NSObject, Q {
  @objc var s: String = ""
  @objc func foo() -> String {}
  @objc subscript(_: String) -> String {
    get {
      return "hi"
    }
    set {}
  }
}

@objc class DynamicIUO : NSObject, Q {
  @objc var t: String! = ""
  @objc func bar() -> String! {}
  @objc subscript(_: DynamicIUO) -> DynamicIUO! {
    get {
      return self
    }
    set {}
  }
}

var dyn = Dynamic()
var dyn_iuo = DynamicIUO()
let s = "hi"
var o: AnyObject = dyn
let _: String = o.s
let _: String = o.s!
let _: String? = o.s
let _: String = o.foo()
let _: String = o.foo!()
let _: String? = o.foo()
let _: String = o[s]
let _: String = o[s]!
let _: String? = o[s]
// FIXME: These should all produce lvalues that we can write through
o.s = s // expected-error {{cannot assign to immutable expression of type 'String?'}}
o.s! = s // expected-error {{cannot assign through '!': 'o' is immutable}}
o[s] = s // expected-error {{cannot assign to immutable expression of type 'String?'}}
o[s]! = s // expected-error {{cannot assign through '!': 'o' is immutable}}

let _: String = o.t
let _: String = o.t!
let _: String = o.t!!
let _: String? = o.t
let _: String = o.bar()
let _: String = o.bar!()
let _: String = o.bar()!
let _: String = o.bar!()!
let _: String? = o.bar()
let _: DynamicIUO = o[dyn_iuo]
let _: DynamicIUO = o[dyn_iuo]!
let _: DynamicIUO = o[dyn_iuo]!!
let _: DynamicIUO? = o[dyn_iuo]
// FIXME: These should all produce lvalues that we can write through
o[dyn_iuo] = dyn_iuo // expected-error {{cannot assign to immutable expression of type 'DynamicIUO??'}}
o[dyn_iuo]! = dyn_iuo // expected-error {{cannot assign through '!': 'o' is immutable}}
o[dyn_iuo]!! = dyn_iuo // expected-error {{cannot assign through '!': 'o' is immutable}}


// Check that we avoid picking an unavailable overload if there's an
// alternative.
class OverloadedWithUnavailable1 {
  @objc func overloadedWithUnavailableA() { }

  @objc
  @available(swift, obsoleted: 3)
  func overloadedWithUnavailableB() { }
}

class OverloadedWithUnavailable2 {
  @available(swift, obsoleted: 3)
  @objc func overloadedWithUnavailableA() { }

  @objc func overloadedWithUnavailableB() { }
}

func testOverloadedWithUnavailable(ao: AnyObject) {
  ao.overloadedWithUnavailableA()
  ao.overloadedWithUnavailableB()
}

func dynamicInitCrash(ao: AnyObject.Type) {
  let sdk = ao.init(blahblah: ())
  // expected-error@-1 {{incorrect argument label in call (have 'blahblah:', expected 'toMemory:')}}
}

// Test that we correctly diagnose ambiguity for different typed properties available
// through dynamic lookup.
@objc protocol P3 {
  var i: UInt { get } // expected-note {{found this candidate}}
  var j: Int { get }
}

class C1 {
  @objc var i: Int { return 0 } // expected-note {{found this candidate}}
  @objc var j: Int { return 0 }
}

_ = (C1() as AnyObject).i // expected-error {{ambiguous use of 'i'}}
_ = (C1() as AnyObject).j // okay

