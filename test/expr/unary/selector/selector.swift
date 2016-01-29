// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-objc-attr-requires-foundation-module -parse %s -verify
import ObjectiveC

// REQUIRES: objc_interop

@objc class A { }
@objc class B { }

class C1 {
  @objc init(a: A, b: B) { }

  @objc func method1(a: A, b: B) { }
  @objc(someMethodWithA:B:) func method2(a: A, b: B) { }

  @objc class func method3(a: A, b: B) { } // expected-note{{found this candidate}}
  @objc class func method3(a a: A, b: B) { } // expected-note{{found this candidate}}

  @objc var a: A = A() // expected-note{{'a' declared here}}

  @objc func getC1() -> AnyObject { return self }
}

@objc protocol P1 {
  func method4(a: A, b: B)
  static func method5(a: B, b: B)
}

extension C1 {
  final func method6() { } // expected-note{{add '@objc' to expose this method to Objective-C}}{{3-3=@objc }}
}

func testSelector(c1: C1, p1: P1, obj: AnyObject) {
  // Instance methods on an instance
  let sel1 = #selector(c1.method1)
  _ = #selector(c1.method1(_:b:))
  _ = #selector(c1.method2)

  // Instance methods on a class.
  _ = #selector(C1.method1)
  _ = #selector(C1.method1(_:b:))
  _ = #selector(C1.method2)

  // Class methods on a class.
  _ = #selector(C1.method3(_:b:))
  _ = #selector(C1.method3(a:b:))

  // Methods on a protocol.
  _ = #selector(P1.method4)
  _ = #selector(P1.method4(_:b:))
  _ = #selector(P1.method5) // FIXME: expected-error{{static member 'method5' cannot be used on instance of type 'P1.Protocol'}}
  _ = #selector(P1.method5(_:b:)) // FIXME: expected-error{{static member 'method5(_:b:)' cannot be used on instance of type 'P1.Protocol'}}
  _ = #selector(p1.method4)
  _ = #selector(p1.method4(_:b:))
  _ = #selector(p1.dynamicType.method5)
  _ = #selector(p1.dynamicType.method5(_:b:))

  // Interesting expressions that refer to methods.
  _ = #selector(Swift.AnyObject.method1)
  _ = #selector(AnyObject.method1!)
  _ = #selector(obj.getC1?().method1)

  // Initializers
  _ = #selector(C1.init(a:b:))

  // Make sure the result has type "ObjectiveC.Selector"
  let sel2: Selector
  sel2 = sel1
  _ = sel2
}

func testAmbiguity() {
  _ = #selector(C1.method3) // expected-error{{ambiguous use of 'method3(_:b:)'}}
}

func testProperties(c1: C1) {
  _ = #selector(c1.a) // expected-error{{argument of '#selector' cannot refer to a property}}
  _ = #selector(C1.a) // FIXME poor diagnostic: expected-error{{instance member 'a' cannot be used on type 'C1'}}
}

func testNonObjC(c1: C1) {
  _ = #selector(c1.method6) // expected-error{{argument of '#selector' refers to a method that is not exposed to Objective-C}}
}

func testParseErrors1() {
  #selector foo // expected-error{{expected '(' following '#selector'}}
}

func testParseErrors2() {
  #selector( // expected-error{{expected expression naming a method within '#selector(...)'}}
}

func testParseErrors3(c1: C1) {
  #selector( // expected-note{{to match this opening '('}}
      c1.method1(_:b:) // expected-error{{expected ')' to complete '#selector' expression}}
}

func testParseErrors4() {
  // Subscripts
  _ = #selector(C1.subscript) // expected-error{{expected member name following '.'}}
  // expected-error@-1{{consecutive statements on a line must be separated by ';'}}
  // expected-error@-2{{expected '(' for subscript parameters}}
}
