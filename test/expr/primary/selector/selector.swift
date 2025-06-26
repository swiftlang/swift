// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-objc-attr-requires-foundation-module -typecheck %s -verify
import ObjectiveC

// REQUIRES: objc_interop

@objc class A { }
@objc class B { }

class C1 {
  @objc init(a: A, b: B) { }

  @objc func method1(_ a: A, b: B) { }
  @objc(someMethodWithA:B:) func method2(_ a: A, b: B) { }

  @objc class func method3(_ a: A, b: B) { } // expected-note{{found this candidate}}
  @objc class func method3(a: A, b: B) { } // expected-note{{found this candidate}}

  @objc(ambiguous1:b:) class func ambiguous(a: A, b: A) { } // expected-note{{found this candidate}}
  @objc(ambiguous2:b:) class func ambiguous(a: A, b: B) { } // expected-note{{found this candidate}}

  @objc func getC1() -> AnyObject { return self }

  @objc func testUnqualifiedSelector(_ a: A, b: B) {
    _ = #selector(testUnqualifiedSelector(_:b:))
    let testUnqualifiedSelector = 1
    _ = #selector(testUnqualifiedSelector(_:b:))
    _ = testUnqualifiedSelector // suppress unused warning
  }

  @objc func testParam(_ testParam: A) { // expected-note{{'testParam' declared here}}
    _ = #selector(testParam) // expected-error{{argument of '#selector' cannot refer to parameter 'testParam'}}
  }

  @objc func testVariable() {
    let testVariable = 1 // expected-note{{'testVariable' declared here}}
    _ = #selector(testVariable) // expected-error{{argument of '#selector' cannot refer to variable 'testVariable'}}
  }
}

@objc protocol P1 {
  func method4(_ a: A, b: B)
  static func method5(_ a: B, b: B)
}

extension C1 {
  final func method6() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}{{3-3=@objc }}
}

func testSelector(_ c1: C1, p1: P1, obj: AnyObject) {
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
  _ = #selector(P1.method5) // expected-error{{static member 'method5' cannot be used on protocol metatype '(any P1).Type'}}
  _ = #selector(P1.method5(_:b:)) // expected-error{{static member 'method5(_:b:)' cannot be used on protocol metatype '(any P1).Type'}}
  _ = #selector(p1.method4)
  _ = #selector(p1.method4(_:b:))
  _ = #selector(type(of: p1).method5)
  _ = #selector(type(of: p1).method5(_:b:))

  // Interesting expressions that refer to methods.
  _ = #selector(Swift.AnyObject.method1)
  _ = #selector(AnyObject.method1!)
  // expected-error@-1 {{cannot force unwrap value of non-optional type '(AnyObject) -> ((A, B) -> ())?'}}
  _ = #selector(obj.getC1?().method1)

  // Initializers
  _ = #selector(C1.init(a:b:))

  // Make sure the result has type "ObjectiveC.Selector"
  let sel2: Selector
  sel2 = sel1
  _ = sel2

  let dict: [Selector: Int] = [:]
  let _: Int? = dict[#selector(c1.method1)]
  let _ = [#selector(c1.method1)]
}

func testAmbiguity() {
  _ = #selector(C1.method3) // expected-error{{ambiguous use of 'method3'}}
  _ = #selector(C1.ambiguous) // expected-error{{ambiguous use of 'ambiguous(a:b:)'}}
}

func testUnusedSelector() {
    #selector(C1.getC1) // expected-warning{{result of '#selector' is unused}}
}

func testNonObjC(_ c1: C1) {
  _ = #selector(c1.method6) // expected-error{{argument of '#selector' refers to instance method 'method6()' that is not exposed to Objective-C}}
}

func testParseErrors1() {
  _ = #selector foo // expected-error{{expected '(' following '#selector'}}
}

func testParseErrors2() {
  _ = #selector( // expected-error{{expected expression naming a method within '#selector(...)'}}
}

func testParseErrors3(_ c1: C1) {
  _ = #selector( // expected-note{{to match this opening '('}}
      c1.method1(_:b:) // expected-error{{expected ')' to complete '#selector' expression}}
}

func testParseErrors4() {
  _ = #selector(C1.subscript) // expected-error{{type 'C1' has no member 'subscript'}}
}

// https://github.com/apple/swift/issues/44436
do {
  let optionalSel: Selector?

  switch optionalSel {
  case #selector(C1.method1)?:
    break
  default:
    break
  }

  @objc class C {
    @objc func bar() {}
  }

  switch optionalSel {
  case #selector(C.bar):
    break
  case #selector(C.bar)!: // expected-error{{cannot force unwrap value of non-optional type 'Selector'}}
    break
  case #selector(C.bar)?:
    break
  default:
    break
  }
}

// https://github.com/apple/swift/issues/51857

protocol SomeProtocol {
  func someFunction()
  func anotherFunction()
}

extension SomeProtocol {
  func someFunction() {
    let _ = #selector(anotherFunction) // expected-error {{cannot use 'anotherFunction' as a selector because protocol 'SomeProtocol' is not exposed to Objective-C}} {{none}}
  }

  func anotherFunction() {
    print("Hello world!")
 }
}

@objc class OverloadedFuncAndProperty {
  @objc static func f() {}
  @objc var f: Int { 0 }
}

func test() -> Selector {
  #selector(OverloadedFuncAndProperty.f)
}

@objc protocol HasThrows {
  @objc optional func doSomething(to object: AnyObject) throws -> Void
}

func testWithThrowing(obj: AnyObject) {
  _ = #selector(HasThrows.doSomething(to:))
}

@available(SwiftStdlib 5.1, *)
@objc protocol HasAsync {
  @objc optional func doSomething(to object: AnyObject) async -> Void
}

@available(SwiftStdlib 5.1, *)
func testWithAsync(obj: AnyObject) {
  _ = #selector(HasAsync.doSomething(to:))
}
