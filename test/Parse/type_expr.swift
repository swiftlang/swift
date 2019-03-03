// RUN: %target-typecheck-verify-swift -swift-version 4
// RUN: %target-typecheck-verify-swift -enable-astscope-lookup -swift-version 4

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
  static func meth() {} // expected-error{{protocol methods must not have bodies}}
  func instMeth() {} // expected-error{{protocol methods must not have bodies}}
}

protocol Bad {
  init() {} // expected-error{{protocol initializers must not have bodies}}
}

struct Gen<T> {
  struct Bar {
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
  _ = Foo.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
               // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = Foo.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
               // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = Foo()
  _ = Foo.prop
  _ = Foo.meth
  let _ : () = Foo.meth()
  _ = Foo.instMeth

  _ = Foo // ok
  _ = Foo.dynamicType // expected-error {{type 'Foo' has no member 'dynamicType'}}
                      // expected-error@-1 {{'.dynamicType' is deprecated. Use 'type(of: ...)' instead}} {{7-7=type(of: }} {{10-22=)}}

  _ = Bad // ok
}

func qualifiedType() {
  _ = Foo.Bar.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                   // expected-note@-1 {{remove '.self' to silence this warning}}
  let _ : Foo.Bar.Type = Foo.Bar.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                      // expected-note@-1 {{remove '.self' to silence this warning}}
  let _ : Foo.Protocol = Foo.self // expected-error{{cannot use 'Protocol' with non-protocol type 'Foo'}}
  _ = Foo.Bar()
  _ = Foo.Bar.prop
  _ = Foo.Bar.meth
  let _ : () = Foo.Bar.meth()
  _ = Foo.Bar.instMeth

  _ = Foo.Bar // ok
  _ = Foo.Bar.dynamicType // expected-error {{type 'Foo.Bar' has no member 'dynamicType'}}
                          // expected-error@-1 {{'.dynamicType' is deprecated. Use 'type(of: ...)' instead}} {{7-7=type(of: }} {{14-26=)}}
}

// We allow '.Type' in expr context
func metaType() {
  let _ = Foo.Type.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                        // expected-note@-1 {{remove '.self' to silence this warning}}
  let _ = Foo.Type.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                        // expected-note@-1 {{remove '.self' to silence this warning}}

  let _ = Foo.Type // ok

  let _ = type(of: Foo.Type) // ok
}

func genType() {
  _ = Gen<Foo>.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                    // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = Gen<Foo>()
  _ = Gen<Foo>.prop
  _ = Gen<Foo>.meth
  let _ : () = Gen<Foo>.meth()
  _ = Gen<Foo>.instMeth
  _ = Gen<Foo> // ok
}

func genQualifiedType() {
  _ = Gen<Foo>.Bar.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                        // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = Gen<Foo>.Bar()
  _ = Gen<Foo>.Bar.prop
  _ = Gen<Foo>.Bar.meth
  let _ : () = Gen<Foo>.Bar.meth()
  _ = Gen<Foo>.Bar.instMeth

  _ = Gen<Foo>.Bar // ok
  _ = Gen<Foo>.Bar.dynamicType // expected-error {{type 'Gen<Foo>.Bar' has no member 'dynamicType'}}
                               // expected-error@-1 {{'.dynamicType' is deprecated. Use 'type(of: ...)' instead}} {{7-7=type(of: }} {{19-31=)}}
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

  // TODO: Errors need improving here.
  _ = type(of: Gen<Foo>.Bar) // expected-error{{argument labels '(of:)' do not match any available overloads}}
                             // expected-note@-1{{overloads for 'type' exist with these partially matching parameter lists: (T.Type), (fo: T.Type)}}
  _ = type(Gen<Foo>.Bar) // ok
  _ = type(of: Gen<Foo>.Bar.self, flag: false) // // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                               // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = type(fo: Foo.Bar.self) // expected-warning {{use of '.self' to reference a type object is deprecated}}
                             // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = type(of: Foo.Bar.self, [1, 2, 3]) // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                        // expected-note@-1 {{remove '.self' to silence this warning}}
}

func archetype<T: Zim>(_: T) {
  _ = T.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
             // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = T()
  // TODO let prop = T.prop
  _ = T.meth
  let _ : () = T.meth()

  _ = T // ok
}

func assocType<T: Zim>(_: T) where T.Zang: Zim {
  _ = T.Zang.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                  // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = T.Zang()
  // TODO _ = T.Zang.prop
  _ = T.Zang.meth
  let _ : () = T.Zang.meth()

  _ = T.Zang // ok
}

class B {
  class func baseMethod() {}
}
class D: B {
  class func derivedMethod() {}
}

func derivedType() {
  let _: B.Type = D.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                         // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = D.baseMethod
  let _ : () = D.baseMethod()

  let _: D.Type = D.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                         // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = D.derivedMethod
  let _ : () = D.derivedMethod()

  let _: B.Type = D // ok
  let _: D.Type = D // ok
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
  let _: P.Protocol = P.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                             // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = P.Type.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                  // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = P.Protocol.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                      // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = P.Protocol.Protocol.self // expected-error{{cannot use 'Protocol' with non-protocol type 'P.Protocol'}}
  _ = P.Protocol.Type.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                           // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = B.Type.self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                  // expected-note@-1 {{remove '.self' to silence this warning}}
}

class E {
  private init() {}
}

func inAccessibleInit() {
  _ = E // ok
}

enum F: Int {
  case A, B
}

struct G {
  var x: Int
}

func implicitInit() {
  _ = F // ok
  _ = G // ok
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

  _ = [1 -> Int]() // expected-error {{expected type before '->'}}
  _ = [Int -> 1]() // expected-error {{expected type after '->'}}
    // expected-error@-1 {{single argument function types require parentheses}}

  // Should parse () as void type when before or after arrow
  _ = [() -> Int]()
  _ = [(Int) -> ()]()

  _ = 2 + () -> Int // expected-error {{expected type before '->'}}
  _ = () -> (Int, Int).2 // expected-error {{expected type after '->'}}
  _ = (Int) -> Int // ok

  _ = @convention(c) () -> Int // ok
  _ = 1 + (@convention(c) () -> Int).self // expected-error{{binary operator '+' cannot be applied to operands of type 'Int' and '(@convention(c) () -> Int).Type'}} // expected-note {{expected an argument list of type '(Int, Int)'}}
  _ = (@autoclosure () -> Int) -> (Int, Int).2 // expected-error {{expected type after '->'}}
  _ = ((@autoclosure () -> Int) -> (Int, Int)).1 // expected-error {{type '(@autoclosure () -> Int) -> (Int, Int)' has no member '1'}}
  _ = ((inout Int) -> Void).self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                 // expected-note@-1 {{remove '.self' to silence this warning}}

  _ = [(Int) throws -> Int]()
  _ = [@convention(swift) (Int) throws -> Int]().count
  _ = [(inout Int) throws -> (inout () -> Void) -> Void]().count
  _ = [String: (@autoclosure (Int) -> Int32) -> Void]().keys // expected-error {{argument type of @autoclosure parameter must be '()'}}
  let _ = [(Int) -> throws Int]() // expected-error{{'throws' may only occur before '->'}}
  let _ = [Int throws Int](); // expected-error{{'throws' may only occur before '->'}} expected-error {{consecutive statements on a line must be separated by ';'}}
}

protocol P1 {}
protocol P2 {}
protocol P3 {}
func compositionType() {
  _ = P1 & P2 // ok
  _ = P1 & P2.self // expected-error {{binary operator '&' cannot be applied to operands of type 'P1.Protocol' and 'P2.Protocol'}} expected-note {{overloads}}
  _ = (P1 & P2).self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                     // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = (P1 & (P2)).self // FIXME: OK? while `typealias P = P1 & (P2)` is rejected.
  _ = (P1 & (P2, P3)).self // expected-error {{non-protocol, non-class type '(P2, P3)' cannot be used within a protocol-constrained type}}
  _ = (P1 & Int).self // expected-error {{non-protocol, non-class type 'Int' cannot be used within a protocol-constrained type}}
  _ = (P1? & P2).self // expected-error {{non-protocol, non-class type 'P1?' cannot be used within a protocol-constrained type}}

  _ = (P1 & P2.Type).self // expected-error {{non-protocol, non-class type 'P2.Type' cannot be used within a protocol-constrained type}}

  _ = P1 & P2 -> P3
  // expected-error @-1 {{single argument function types require parentheses}} {{7-7=(}} {{14-14=)}}

  _ = P1 & P2 -> P3 & P1 -> Int
  // expected-error @-1 {{single argument function types require parentheses}} {{18-18=(}} {{25-25=)}}
  // expected-error @-2 {{single argument function types require parentheses}} {{7-7=(}} {{14-14=)}}

  _ = (() -> P1 & P2).self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                           // expected-note@-1 {{remove '.self' to silence this warning}}
  _ = (P1 & P2 -> P3 & P2).self // expected-error {{single argument function types require parentheses}} {{8-8=(}} {{15-15=)}}
  _ = ((P1 & P2) -> (P3 & P2) -> P1 & Any).self // expected-warning {{use of '.self' to reference a type object is deprecated}}
                                                // expected-note@-1 {{remove '.self' to silence this warning}}
}

func complexSequence() {
  // (assign_expr
  //   (discard_assignment_expr)
  //   (try_expr
  //     (type_expr typerepr='P1 & P2 throws -> P3 & P1')))
  _ = try P1 & P2 throws -> P3 & P1
  // expected-warning @-1 {{no calls to throwing functions occur within 'try' expression}}
  // expected-error @-2 {{single argument function types require parentheses}} {{none}} {{11-11=(}} {{18-18=)}}
}

func takesVoid(f: Void -> ()) {} // expected-error {{single argument function types require parentheses}} {{19-23=()}}

func takesOneArg<T>(_: T.Type) {}
func takesTwoArgs<T>(_: T.Type, _: Int) {}

func testMissingSelf() {
  // None of these were not caught in Swift 3.
  // See test/Compatibility/type_expr.swift.

  takesOneArg(Int) // ok

  takesOneArg(Swift.Int) // ok

  takesTwoArgs(Int, 0) // ok

  takesTwoArgs(Swift.Int, 0) // ok

  Swift.Int // expected-warning {{expression of type 'Int.Type' is unused}}

  _ = Swift.Int // ok
}
