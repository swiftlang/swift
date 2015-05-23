// RUN: %target-parse-verify-swift

// REQUIRES: objc_interop

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

  @objc func foo(i: Int) { }
  @objc func bar() { }

  @objc func ovl2() -> A { } // expected-note{{found this candidate}}

  @objc func ovl4() -> B { }
  @objc func ovl5() -> B { } // expected-note{{found this candidate}}

  @objc class func staticFoo(i : Int) { }

  @objc func prop3() -> Int { return 5 }
}

class Y : P {
  init() {} 

  @objc func foo(s: String) { }
  @objc func wibble() { }

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
  func generic4<T>(x : T) { }
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
  func generic1(x : T) { }
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
obj.wobble!() // expected-error{{'Id' does not have a member named 'wobble()'}}
obj.ext1!()
obj.wonka!()

// Same as above but without the '!'
obj.bar()
obj.foo(5)
obj.foo("hello")
obj.wibble()
obj.wobble() // expected-error{{'Id' does not have a member named 'wobble()'}}
obj.ext1()
obj.wonka()

// Find class methods via dynamic method lookup.
obj.dynamicType.staticFoo!(5)
obj.dynamicType.staticWibble!()

// Same as above but without the '!'
obj.dynamicType.staticFoo(5)
obj.dynamicType.staticWibble()

// Overloading and ambiguity resolution

// When we have overriding, pick the least restrictive declaration.
var ovl1Result = obj.ovl1!()
ovl1Result = A() // verify that we got an A, not a B

// Same as above but without the '!'
obj.ovl1()

// Don't allow overload resolution between declarations from different
// classes.
var ovl2ResultA = obj.ovl2!() // expected-error{{ambiguous use of 'ovl2'}}

// ... but it's okay to allow overload resolution between declarations
// from the same class.
var ovl3Result = obj.ovl3!()
ovl3Result = B()

// For [objc] declarations, we can ignore declarations with the same
// selector and type.
var ovl4Result = obj.ovl4!()

// ... but not when the types are different.
var ovl5Result = obj.ovl5!() // expected-error{{ambiguous use of 'ovl5'}}

// Same as above but without the '!'
obj.ovl4()

// Generics

// Dynamic lookup cannot find members of a generic class (or a class
// that's a member of anything generic), because we wouldn't be able
// to figure out the generic arguments.
var generic1Result = obj.generic1!(17) // expected-error{{'Id' does not have a member named 'generic1'}}
obj.generic2!() // expected-error{{'Id' does not have a member named 'generic2()'}}
obj.generic3!() // expected-error{{'Id' does not have a member named 'generic3()'}}

// Dynamic method lookup can't find non-[objc] members
obj.generic4!(5) // expected-error{{'Id' does not have a member named 'generic4'}}

// Find properties via dynamic lookup.
var prop1Result : Int = obj.prop1!
var prop2Result : String = obj.prop2!
obj.prop2 = "hello" // expected-error{{cannot assign to property: 'obj' is immutable}}
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

var obj2 : protocol<AnyObject, P> = Y()

class Z2 : AnyObject { }
class Z3<T : AnyObject> { }
class Z4<T where T : AnyObject> { }

// Don't allow one to call instance methods on the Type via
// dynamic method lookup.
obj.dynamicType.foo!(obj)(5) // expected-error{{'Id.Type' does not have a member named 'foo'}}

// Checked casts to AnyObject
var p: P = Y()
var obj3 : AnyObject = (p as! AnyObject)! // expected-error{{extraneous postfix '!'}}

// Implicit force of an implicitly unwrapped optional
let uopt : AnyObject! = nil
uopt.wibble!()
