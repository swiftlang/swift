// RUN: %target-typecheck-verify-swift

// REQUIRES: objc_interop

func markUsed<T>(_ t: T) {}

class A {
  @objc func do_a() {}

  @objc(do_b_2:) func do_b(_ x: Int) {}
  @objc func do_b(_ x: Float) {}

  @objc func do_c(x: Int) {}
  @objc func do_c(y: Int) {}
}

func test0(_ a: AnyObject) {
  a.do_a?()

  a.do_b?(1)
  a.do_b?(5.0)

  a.do_c?(1) // expected-error {{cannot invoke value of function type with argument list '(Int)'}}
  a.do_c?(x: 1)
}

func test1(_ a: A) {
  a?.do_a() // expected-error {{cannot use optional chaining on non-optional value of type 'A'}} {{4-5=}}
  a!.do_a() // expected-error {{cannot force unwrap value of non-optional type 'A'}} {{4-5=}}
  // Produce a specialized diagnostic here?
  a.do_a?() // expected-error {{cannot use optional chaining on non-optional value of type '() -> ()'}} {{9-10=}}
}

// <rdar://problem/15508756>
extension Optional {
  func bind<U>(_ f: (Wrapped) -> U?) -> U? {
    switch self {
    case .some(let x):
      return f(x)
    case .none:
      return .none
    }
  }
}

var c: String? = Optional<Int>(1)
  .bind {(x: Int) in markUsed("\(x)!"); return "two" }

func test4() {
  func foo() -> Int { return 0 }
  func takes_optfn(_ f : () -> Int?) -> Int? { return f() }

  _ = takes_optfn(foo)

  func takes_objoptfn(_ f : () -> AnyObject?) -> AnyObject? { return f() }
  func objFoo() -> AnyObject { return A() }
  _ = takes_objoptfn(objFoo) // okay
  func objBar() -> A { return A() }
  _ = takes_objoptfn(objBar) // okay
}

func test5() -> Int? {
  return nil
}

func test6<T>(_ x : T) {
  // FIXME: this code should work; T could be Int? or Int??
  // or something like that at runtime.  rdar://16374053
  _ = x as? Int? // expected-error {{cannot downcast from 'T' to a more optional type 'Int?'}}
}

class B : A { }

func test7(_ x : A) {
  _ = x as? B? // expected-error{{cannot downcast from 'A' to a more optional type 'B?'}}
}

func test8(_ x : AnyObject?) {
  let _ : A = x as! A
}


// Partial ordering with optionals
func test9_helper<T: P>(_ x: T) -> Int { }
func test9_helper<T: P>(_ x: T?) -> Double { }

func test9_helper2<T>(_ x: T) -> Int { }
func test9_helper2<T>(_ x: T?) -> Double { }

func test9(_ i: Int, io: Int?) {
  let result = test9_helper(i)
  var _: Int = result
  let result2 = test9_helper(io)
  let _: Double = result2

  let result3 = test9_helper2(i)
  var _: Int = result3
  let result4 = test9_helper2(io)
  let _: Double = result4
}

protocol P { }

func test10_helper<T : P>(_ x: T) -> Int { }
func test10_helper<T : P>(_ x: T?) -> Double { }

extension Int : P { }

func test10(_ i: Int, io: Int?) {
  let result = test10_helper(i)
  var _: Int = result

  let result2 = test10_helper(io)
  var _: Double = result2
}

var z: Int? = nil
z = z ?? 3

var fo: Float? = 3.14159

func voidOptional(_ handler: () -> ()?) {}
func testVoidOptional() {
  let noop: () -> Void = {}
  voidOptional(noop)

  let optNoop: (()?) -> ()? = { return $0 }
  voidOptional(optNoop)
}

protocol Proto1 {}
protocol Proto2 {}
struct Nilable: ExpressibleByNilLiteral {
	init(nilLiteral: ()) {}
}
func testTernaryWithNil<T>(b: Bool, s: String, i: Int, a: Any, t: T, m: T.Type, p: Proto1 & Proto2, arr: [Int], opt: Int?, iou: Int!, n: Nilable) {
  let t1 = b ? s : nil
  let _: Double = t1 // expected-error{{value of type 'String?'}}
  let t2 = b ? nil : i
  let _: Double = t2 // expected-error{{value of type 'Int?'}}
  let t3 = b ? "hello" : nil
  let _: Double = t3 // expected-error{{value of type 'String?'}}
  let t4 = b ? nil : 1
  let _: Double = t4 // expected-error{{value of type 'Int?'}}
  let t5 = b ? (s, i) : nil
  let _: Double = t5 // expected-error{{value of type '(String, Int)?}}
  let t6 = b ? nil : (i, s)
  let _: Double = t6 // expected-error{{value of type '(Int, String)?}}
  let t7 = b ? ("hello", 1) : nil
  let _: Double = t7 // expected-error{{value of type '(String, Int)?}}
  let t8 = b ? nil : (1, "hello")
  let _: Double = t8 // expected-error{{value of type '(Int, String)?}}
  let t9 = b ? { $0 * 2 } : nil
  let _: Double = t9 // expected-error{{value of type '((Int) -> Int)?}}
  let t10 = b ? nil : { $0 * 2 }
  let _: Double = t10 // expected-error{{value of type '((Int) -> Int)?}}
  let t11 = b ? a : nil
  let _: Double = t11 // expected-error{{value of type 'Any?'}}
  let t12 = b ? nil : a
  let _: Double = t12 // expected-error{{value of type 'Any?'}}
  let t13 = b ? t : nil
  let _: Double = t13 // expected-error{{value of type 'T?'}}
  let t14 = b ? nil : t
  let _: Double = t14 // expected-error{{value of type 'T?'}}
  let t15 = b ? m : nil
  let _: Double = t15 // expected-error{{value of type 'T.Type?'}}
  let t16 = b ? nil : m
  let _: Double = t16 // expected-error{{value of type 'T.Type?'}}
  let t17 = b ? p : nil
  let _: Double = t17 // expected-error{{value of type '(Proto1 & Proto2)?'}}
  let t18 = b ? nil : p
  let _: Double = t18 // expected-error{{value of type '(Proto1 & Proto2)?'}}
  let t19 = b ? arr : nil
  let _: Double = t19 // expected-error{{value of type '[Int]?'}}
  let t20 = b ? nil : arr
  let _: Double = t20 // expected-error{{value of type '[Int]?'}}
  let t21 = b ? opt : nil
  let _: Double = t21 // expected-error{{value of type 'Int?'}}
  let t22 = b ? nil : opt
  let _: Double = t22 // expected-error{{value of type 'Int?'}}
  let t23 = b ? iou : nil
  let _: Double = t23 // expected-error{{value of type 'Int?'}}
  let t24 = b ? nil : iou
  let _: Double = t24 // expected-error{{value of type 'Int?'}}
  let t25 = b ? n : nil
  let _: Double = t25 // expected-error{{value of type 'Nilable'}}
  let t26 = b ? nil : n
  let _: Double = t26 // expected-error{{value of type 'Nilable'}}
}

// inference with IUOs
infix operator ++++

protocol PPPP {
  static func ++++(x: Self, y: Self) -> Bool
}

func compare<T: PPPP>(v: T, u: T!) -> Bool {
  return v ++++ u
}

func sr2752(x: String?, y: String?) {
  _ = x.map { xx in
    y.map { _ in "" } ?? "\(xx)"
  }
}

// SR-3248 - Invalid diagnostic calling implicitly unwrapped closure
var sr3248 : ((Int) -> ())!
sr3248?(a: 2) // expected-error {{extraneous argument label 'a:' in call}}
sr3248!(a: 2) // expected-error {{extraneous argument label 'a:' in call}}
sr3248(a: 2)  // expected-error {{extraneous argument label 'a:' in call}}

struct SR_3248 {
    var callback: (([AnyObject]) -> Void)!
}

SR_3248().callback?("test") // expected-error {{cannot convert value of type 'String' to expected argument type '[AnyObject]'}}
SR_3248().callback!("test") // expected-error {{cannot convert value of type 'String' to expected argument type '[AnyObject]'}}
SR_3248().callback("test")  // expected-error {{cannot invoke 'callback' with an argument list of type '(String)'}}

_? = nil  // expected-error {{'nil' requires a contextual type}}
_?? = nil // expected-error {{'nil' requires a contextual type}}


// rdar://problem/29993596
func takeAnyObjects(_ lhs: AnyObject?, _ rhs: AnyObject?) { }

infix operator !====

func !====(_ lhs: AnyObject?, _ rhs: AnyObject?) -> Bool { return false }

func testAnyObjectImplicitForce(lhs: AnyObject?!, rhs: AnyObject?) {
  if lhs !==== rhs { }

  takeAnyObjects(lhs, rhs)
}

// SR-4056
protocol P1 { }

class C1: P1 { }

protocol P2 {
    var prop: C1? { get }
}

class C2 {
    var p1: P1?
    var p2: P2?

    var computed: P1? {
        return p1 ?? p2?.prop
    }
}


// rdar://problem/31779785
class X { }

class Bar {
  let xOpt: X?
  let b: Bool

  init() {
    let result = b ? nil : xOpt
    let _: Int = result // expected-error{{cannot convert value of type 'X?' to specified type 'Int'}}
  }
}

// rdar://problem/37508855
func rdar37508855(_ e1: X?, _ e2: X?) -> [X] {
  return [e1, e2].filter { $0 == nil }.map { $0! }
}

func se0213() {
  struct Q: ExpressibleByStringLiteral {
    typealias StringLiteralType =  String

    var foo: String

    init?(_ possibleQ: StringLiteralType) {
      return nil
    }

    init(stringLiteral str: StringLiteralType) {
      self.foo = str
    }
  }

  _ = Q("why")?.foo // Ok
  _ = Q("who")!.foo // Ok
  _ = Q?("how") // Ok
}

func rdar45218255(_ i: Int) {
  struct S<T> {
    init(_:[T]) {}
  }

  _ = i!           // expected-error {{cannot force unwrap value of non-optional type 'Int'}} {{8-9=}}
  _ = [i!]         // expected-error {{cannot force unwrap value of non-optional type 'Int'}} {{9-10=}}
  _ = S<Int>([i!]) // expected-error {{cannot force unwrap value of non-optional type 'Int'}} {{16-17=}}
}
