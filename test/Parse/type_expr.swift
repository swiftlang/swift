// RUN: %target-parse-verify-swift

// Types in expression contexts must be followed by a member access or
// constructor call.

struct Foo {
  struct Bar {
    init() {}
    static var prop: Int = 0
    static func meth() {}
    func instMeth() {}
  }
  init() {}
  static var prop: Int = 0
  static func meth() {}
  func instMeth() {}
}

protocol Zim {
  associatedtype Zang

  init()
  // TODO class var prop: Int { get }
  static func meth() {} // expected-error{{protocol methods may not have bodies}}
  func instMeth() {} // expected-error{{protocol methods may not have bodies}}
}

protocol Bad {
  init() {} // expected-error{{protocol initializers may not have bodies}}
}

struct Gen<T> {
  struct Bar { // expected-error{{nested in generic type}}
    init() {}
    static var prop: Int { return 0 }
    static func meth() {}
    func instMeth() {}
  }

  init() {}
  static var prop: Int { return 0 }
  static func meth() {}
  func instMeth() {}
}

func unqualifiedType() {
  _ = Foo.self
  _ = Foo.self
  _ = Foo()
  _ = Foo.prop
  _ = Foo.meth
  let _ : () = Foo.meth()
  _ = Foo.instMeth

  _ = Foo // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} {{10-10=()}} expected-note{{use '.self'}} {{10-10=.self}}
  _ = Foo.dynamicType // expected-error {{type 'Foo' has no member 'dynamicType'}}
                      // expected-error@-1 {{'.dynamicType' is deprecated. Use 'type(of: ...)' instead}} {{7-7=type(of: }} {{10-22=)}}

  _ = Bad // expected-error{{expected member name or constructor call after type name}}
  // expected-note@-1{{use '.self' to reference the type object}}{{10-10=.self}}
}

func qualifiedType() {
  _ = Foo.Bar.self
  let _ : Foo.Bar.Type = Foo.Bar.self
  let _ : Foo.Protocol = Foo.self // expected-error{{cannot use 'Protocol' with non-protocol type 'Foo'}}
  _ = Foo.Bar()
  _ = Foo.Bar.prop
  _ = Foo.Bar.meth
  let _ : () = Foo.Bar.meth()
  _ = Foo.Bar.instMeth

  _ = Foo.Bar // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} {{14-14=()}} expected-note{{use '.self'}} {{14-14=.self}}
  _ = Foo.Bar.dynamicType // expected-error {{type 'Foo.Bar' has no member 'dynamicType'}}
                          // expected-error@-1 {{'.dynamicType' is deprecated. Use 'type(of: ...)' instead}} {{7-7=type(of: }} {{14-26=)}}
}

/* TODO allow '.Type' in expr context
func metaType() {
  let ty = Foo.Type.self
  let metaTy = Foo.Type.self

  let badTy = Foo.Type
  let badMetaTy = type(of: Foo.Type)
}
 */

func genType() {
  _ = Gen<Foo>.self
  _ = Gen<Foo>()
  _ = Gen<Foo>.prop
  _ = Gen<Foo>.meth
  let _ : () = Gen<Foo>.meth()
  _ = Gen<Foo>.instMeth
  _ = Gen<Foo> // expected-error{{expected member name or constructor call after type name}}
               // expected-note@-1{{use '.self' to reference the type object}}
               // expected-note@-2{{add arguments after the type to construct a value of the type}}
}

func genQualifiedType() {
  _ = Gen<Foo>.Bar.self
  _ = Gen<Foo>.Bar()
  _ = Gen<Foo>.Bar.prop
  _ = Gen<Foo>.Bar.meth
  let _ : () = Gen<Foo>.Bar.meth()
  _ = Gen<Foo>.Bar.instMeth

  _ = Gen<Foo>.Bar
  _ = Gen<Foo>.Bar.dynamicType // expected-error {{'.dynamicType' is deprecated. Use 'type(of: ...)' instead}} {{7-7=type(of: }} {{19-31=)}} 
}

func typeOfShadowing() {
  // Try to shadow type(of:)
  func type<T>(of t: T.Type, flag: Bool) -> T.Type {
    return t
  }

  func type<T, U>(of t: T.Type, _ : U) -> T.Type {
    return t
  }
  
  func type<T>(_ t: T.Type) -> T.Type {
    return t
  }

  func type<T>(fo t: T.Type) -> T.Type {
    return t
  }

  _ = type(of: Gen<Foo>.Bar) // No error here.
  _ = type(Gen<Foo>.Bar) // No error here. 
  _ = type(of: Gen<Foo>.Bar.self, flag: false) // No error here.
  _ = type(fo: Foo.Bar.self) // No error here.
  _ = type(of: Foo.Bar.self, [1, 2, 3]) // No error here.
}

func archetype<T: Zim>(_: T) {
  _ = T.self
  _ = T()
  // TODO let prop = T.prop
  _ = T.meth
  let _ : () = T.meth()

  _ = T // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} {{8-8=()}} expected-note{{use '.self'}} {{8-8=.self}}
}

func assocType<T: Zim>(_: T) where T.Zang: Zim {
  _ = T.Zang.self
  _ = T.Zang()
  // TODO _ = T.Zang.prop
  _ = T.Zang.meth
  let _ : () = T.Zang.meth()

  _ = T.Zang // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} {{13-13=()}} expected-note{{use '.self'}} {{13-13=.self}}
}

class B {
  class func baseMethod() {}
}
class D: B {
  class func derivedMethod() {}
}

func derivedType() {
  let _: B.Type = D.self
  _ = D.baseMethod
  let _ : () = D.baseMethod()

  let _: D.Type = D.self
  _ = D.derivedMethod
  let _ : () = D.derivedMethod()

  let _: B.Type = D // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} {{20-20=()}} expected-note{{use '.self'}} {{20-20=.self}}
  let _: D.Type = D // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments}} {{20-20=()}} expected-note{{use '.self'}} {{20-20=.self}}
}

// Referencing a nonexistent member or constructor should not trigger errors
// about the type expression.
func nonexistentMember() {
  let cons = Foo("this constructor does not exist") // expected-error{{argument passed to call that takes no arguments}}
  let prop = Foo.nonexistent // expected-error{{type 'Foo' has no member 'nonexistent'}}
  let meth = Foo.nonexistent() // expected-error{{type 'Foo' has no member 'nonexistent'}}
}

protocol P {}

func meta_metatypes() {
  let _: P.Protocol = P.self
  _ = P.Type.self
  _ = P.Protocol.self
  _ = P.Protocol.Protocol.self // expected-error{{cannot use 'Protocol' with non-protocol type 'P.Protocol'}}
  _ = P.Protocol.Type.self
  _ = B.Type.self
}

// https://bugs.swift.org/browse/SR-502
func testFunctionCollectionTypes() {
  _ = [(Int) -> Int]()
  _ = [(Int, Int) -> Int]()
  _ = [(x: Int, y: Int) -> Int]()
  // Make sure associativity is correct
  let a = [(Int) -> (Int) -> Int]()
  let b: Int = a[0](5)(4)

  _ = [String: (Int) -> Int]()
  _ = [String: (Int, Int) -> Int]()

  _ = [1 -> Int]() // expected-error{{expected type before '->'}}
  _ = [Int -> 1]() // expected-error{{expected type after '->'}}

  // Should parse () as void type when before or after arrow
  _ = [() -> Int]()
  _ = [(Int) -> ()]()

  _ = [(Int) throws -> Int]()
  _ = [(Int) -> throws Int]() // expected-error{{'throws' may only occur before '->'}}
  _ = [Int throws Int]() // expected-error{{'throws' may only occur before '->'}}

  let _ = (Int) -> Int // expected-error{{expected member name or constructor call after type name}} expected-note{{add arguments after the type to construct a value of the type}} expected-note{{use '.self' to reference the type object}}
  let _ = 2 + () -> Int // expected-error{{expected type before '->'}}
  let _ = () -> (Int, Int).2 // expected-error{{expected type after '->'}}
}
