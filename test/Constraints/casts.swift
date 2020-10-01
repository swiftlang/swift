// RUN: %target-typecheck-verify-swift -enable-objc-interop

class B { 
  init() {} 
}
class D : B {
  override init() { super.init() }
}

var seven : Double = 7

var pair : (Int, Double) = (1, 2)

var closure : (Int, Int) -> Int = { $0 + $1 }

var d_as_b : B = D()
var b_as_d = B() as! D
var bad_b_as_d : D = B()  // expected-error{{cannot convert value of type 'B' to specified type 'D'}}

var d = D()
var b = B()

var d_as_b_2 : B = d 
var b_as_d_2 = b as! D

var b_is_d:Bool = B() is D
// FIXME: Poor diagnostic below.
var bad_d_is_b:Bool = D() is B // expected-warning{{always true}}

func base_class_archetype_casts<T : B>(_ t: T) {
  var _ : B = t
  _ = B() as! T
  var _ : T = B() // expected-error{{cannot convert value of type 'B' to specified type 'T'}}

  let b = B()

  _ = b as! T

  var _:Bool = B() is T
  var _:Bool = b is T
  var _:Bool = t is B // expected-warning{{always true}}

  _ = t as! D
}

protocol P1 { func p1() }
protocol P2 { func p2() }

struct S1 : P1 {
  func p1() {}
}
class C1 : P1 {
  func p1() {}
}
class D1 : C1 {}

struct S2 : P2 {
  func p2() {}
}

struct S3 {}

struct S12 : P1, P2 {
  func p1() {}
  func p2() {}
}

func protocol_archetype_casts<T : P1>(_ t: T, p1: P1, p2: P2, p12: P1 & P2) {
  // Coercions.
  var _ : P1 = t
  var _ : P2 = t // expected-error{{value of type 'T' does not conform to specified type 'P2'}}

  // Checked unconditional casts.
  _ = p1 as! T
  _ = p2 as! T
  _ = p12 as! T

  _ = t as! S1
  _ = t as! S12
  _ = t as! C1
  _ = t as! D1

  _ = t as! S2

  _ = S1() as! T
  _ = S12() as! T
  _ = C1() as! T
  _ = D1() as! T

  _ = S2() as! T

  // Type queries.
  var _:Bool = p1 is T
  var _:Bool = p2 is T
  var _:Bool = p12 is T

  var _:Bool = t is S1
  var _:Bool = t is S12
  var _:Bool = t is C1
  var _:Bool = t is D1

  var _:Bool = t is S2
}

func protocol_concrete_casts(_ p1: P1, p2: P2, p12: P1 & P2) {
  // Checked unconditional casts.
  _ = p1 as! S1
  _ = p1 as! C1
  _ = p1 as! D1
  _ = p1 as! S12

  _ = p1 as! P1 & P2

  _ = p2 as! S1 // expected-warning {{cast from 'P2' to unrelated type 'S1' always fails}}

  _ = p12 as! S1 // expected-warning {{cast from 'P1 & P2' to unrelated type 'S1' always fails}}
  _ = p12 as! S2 // expected-warning {{cast from 'P1 & P2' to unrelated type 'S2' always fails}}
  _ = p12 as! S12
  _ = p12 as! S3 // expected-warning {{cast from 'P1 & P2' to unrelated type 'S3' always fails}}

  // Type queries.
  var _:Bool = p1 is S1
  var _:Bool = p1 is C1
  var _:Bool = p1 is D1
  var _:Bool = p1 is S12

  var _:Bool = p1 is P1 & P2

  var _:Bool = p2 is S1 // expected-warning {{cast from 'P2' to unrelated type 'S1' always fails}}

  var _:Bool = p12 is S1 // expected-warning {{cast from 'P1 & P2' to unrelated type 'S1' always fails}}
  var _:Bool = p12 is S2 // expected-warning {{cast from 'P1 & P2' to unrelated type 'S2' always fails}}
  var _:Bool = p12 is S12
  var _:Bool = p12 is S3 // expected-warning {{cast from 'P1 & P2' to unrelated type 'S3' always fails}}
}

func conditional_cast(_ b: B) -> D? {
  return b as? D
}

@objc protocol ObjCProto1 {}
@objc protocol ObjCProto2 {}
protocol NonObjCProto : class {}

@objc class ObjCClass {}
class NonObjCClass {}

func objc_protocol_casts(_ op1: ObjCProto1, opn: NonObjCProto) {
  _ = ObjCClass() as! ObjCProto1
  _ = ObjCClass() as! ObjCProto2
  _ = ObjCClass() as! ObjCProto1 & ObjCProto2
  _ = ObjCClass() as! NonObjCProto
  _ = ObjCClass() as! ObjCProto1 & NonObjCProto

  _ = op1 as! ObjCProto1 & ObjCProto2
  _ = op1 as! ObjCProto2
  _ = op1 as! ObjCProto1 & NonObjCProto
  _ = opn as! ObjCProto1

  _ = NonObjCClass() as! ObjCProto1
}

func dynamic_lookup_cast(_ dl: AnyObject) {
  _ = dl as! ObjCProto1
  _ = dl as! ObjCProto2
  _ = dl as! ObjCProto1 & ObjCProto2
}

// Cast to subclass with generic parameter inference
class C2<T> : B { }
class C3<T> : C2<[T]> { 
  func f(_ x: T) { }
}
var c2i : C2<[Int]> = C3()
var c3iOpt = c2i as? C3
c3iOpt?.f(5)
var b1 = c2i is C3
var c2f: C2<Float>? = b as? C2
var c2f2: C2<[Float]>? = b as! C3


// <rdar://problem/15633178>
var f: (Float) -> Float = { $0 as Float }
var f2: (B) -> Bool = { $0 is D }

func metatype_casts<T, U>(_ b: B.Type, t:T.Type, u: U.Type) {
  _ = b is D.Type
  _ = T.self is U.Type
  _ = type(of: T.self) is U.Type.Type
  _ = type(of: b) is D.Type // expected-warning{{always fails}}
  _ = b is D.Type.Type // expected-warning{{always fails}}

}

// <rdar://problem/17017851>
func forcedDowncastToOptional(_ b: B) {
  var dOpt: D? = b as! D // expected-warning{{treating a forced downcast to 'D' as optional will never produce 'nil'}}
  // expected-note@-1{{use 'as?' to perform a conditional downcast to 'D'}}{{22-23=?}}
  // expected-note@-2{{add parentheses around the cast to silence this warning}}{{18-18=(}}{{25-25=)}}
  dOpt = b as! D // expected-warning{{treating a forced downcast to 'D' as optional will never produce 'nil'}}
  // expected-note@-1{{use 'as?' to perform a conditional downcast to 'D'}}{{14-15=?}}
  // expected-note@-2{{add parentheses around the cast to silence this warning}}{{10-10=(}}{{17-17=)}}
  dOpt = (b as! D)
  _ = dOpt
}

_ = b1 as Int    // expected-error {{cannot convert value of type 'Bool' to type 'Int' in coercion}}
_ = seven as Int // expected-error {{cannot convert value of type 'Double' to type 'Int' in coercion}}

func rdar29894174(v: B?) {
  let _ = [v].compactMap { $0 as? D }
}

// When re-typechecking a solution with an 'is' cast applied,
// we would fail to produce a diagnostic.
func process(p: Any?) {
  compare(p is String)
  // expected-error@-1 {{missing argument for parameter #2 in call}} {{22-22=, <#Bool#>}}
}

func compare<T>(_: T, _: T) {} // expected-note {{'compare' declared here}}
func compare<T>(_: T?, _: T?) {}

_ = nil? as? Int?? // expected-error {{'nil' requires a contextual type}}

func test_tuple_casts_no_warn() {
  struct Foo {}

  let arr: [(Any, Any)] = [(Foo(), Foo())]
  let tup: (Any, Any) = (Foo(), Foo())

  _ = arr as! [(Foo, Foo)] // Ok
  _ = tup as! (Foo, Foo) // Ok

  _ = arr as! [(Foo, Foo, Foo)] // Ok
  _ = tup as! (Foo, Foo, Foo) // expected-warning {{cast from '(Any, Any)' to unrelated type '(Foo, Foo, Foo)' always fails}}

  _ = arr as! [(a: Foo, Foo)] // Ok
  _ = tup as! (a: Foo, Foo) // Ok
}

infix operator ^^^
func ^^^ <T> (lhs: T?, rhs: @autoclosure () -> T) -> T { lhs! }
func ^^^ <T> (lhs: T?, rhs: @autoclosure () -> T?) -> T? { lhs! }

func ohno<T>(_ x: T) -> T? { nil }

// SR-12369: Make sure we don't drop the coercion constraint.
func test_coercions_with_overloaded_operator(str: String, optStr: String?, veryOptString: String????) {
  _ = (str ?? "") as String // expected-warning {{left side of nil coalescing operator '??' has non-optional type 'String', so the right side is never used}}
  _ = (optStr ?? "") as String

  _ = (str ?? "") as Int // expected-error {{cannot convert value of type 'String' to type 'Int' in coercion}}
  _ = (optStr ?? "") as Int // expected-error {{cannot convert value of type 'String' to type 'Int' in coercion}}
  _ = (optStr ?? "") as Int? // expected-error {{'String' is not convertible to 'Int?'; did you mean to use 'as!' to force downcast?}}

  _ = (str ^^^ "") as Int // expected-error {{cannot convert value of type 'String' to type 'Int' in coercion}}
  _ = (optStr ^^^ "") as Int // expected-error {{cannot convert value of type 'String' to type 'Int' in coercion}}
  _ = (optStr ^^^ "") as Int? // expected-error {{'String' is not convertible to 'Int?'; did you mean to use 'as!' to force downcast?}}

  _ = ([] ?? []) as String // expected-error {{cannot convert value of type '[Any]' to type 'String' in coercion}}
  _ = ([""] ?? []) as [Int: Int] // expected-error {{cannot convert value of type '[String]' to type '[Int : Int]' in coercion}}
  _ = (["": ""] ?? [:]) as [Int] // expected-error {{cannot convert value of type '[String : String]' to type '[Int]' in coercion}}
  _ = (["": ""] ?? [:]) as Set<Int> // expected-error {{cannot convert value of type '[String : String]' to type 'Set<Int>' in coercion}}
  _ = (["": ""] ?? [:]) as Int? // expected-error {{cannot convert value of type '[String : String]' to type 'Int?' in coercion}}

  _ = ("" ?? "" ?? "") as String // expected-warning 2{{left side of nil coalescing operator '??' has non-optional type 'String', so the right side is never used}}
  _ = ((veryOptString ?? "") ?? "") as String??
  _ = ((((veryOptString ?? "") ?? "") ?? "") ?? "") as String

  _ = ("" ?? "" ?? "") as Float // expected-error {{cannot convert value of type 'String' to type 'Float' in coercion}}
  _ = ((veryOptString ?? "") ?? "") as [String] // expected-error {{cannot convert value of type 'String??' to type '[String]' in coercion}}
  _ = ((((veryOptString ?? "") ?? "") ?? "") ?? "") as [String] // expected-error {{cannot convert value of type 'String' to type '[String]' in coercion}}

  _ = ohno(ohno(ohno(str))) as String???
  _ = ohno(ohno(ohno(str))) as Int // expected-error {{cannot convert value of type 'String???' to type 'Int' in coercion}}
}

func id<T>(_ x: T) -> T { x }

func test_compatibility_coercions(_ arr: [Int], _ optArr: [Int]?, _ dict: [String: Int], _ set: Set<Int>) {
  // Successful coercions don't raise a warning.
  _ = arr as [Any]?
  _ = dict as [String: Int]?
  _ = set as Set<Int>

  // Don't fix the simple case where no type variable is introduced, that was
  // always disallowed.
  _ = arr as [String] // expected-error {{cannot convert value of type '[Int]' to type '[String]' in coercion}}
  // expected-note@-1 {{arguments to generic parameter 'Element' ('Int' and 'String') are expected to be equal}}
  _ = dict as [String: String] // expected-error {{cannot convert value of type '[String : Int]' to type '[String : String]' in coercion}}
  // expected-note@-1 {{arguments to generic parameter 'Value' ('Int' and 'String') are expected to be equal}}
  _ = dict as [String: String]? // expected-error {{'[String : Int]' is not convertible to '[String : String]?'; did you mean to use 'as!' to force downcast?}}
  _ = (dict as [String: Int]?) as [String: Int] // expected-error {{value of optional type '[String : Int]?' must be unwrapped to a value of type '[String : Int]'}}
  // expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
  // expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
  _ = set as Set<String>  // expected-error {{cannot convert value of type 'Set<Int>' to type 'Set<String>' in coercion}}
   // expected-note@-1 {{arguments to generic parameter 'Element' ('Int' and 'String') are expected to be equal}}

  // Apply the compatibility logic when a type variable is introduced. It's
  // unfortunate that this means we'll temporarily accept code we didn't before,
  // but it at least means we shouldn't break compatibility with anything.
  _ = id(arr) as [String] // expected-warning {{coercion from '[Int]' to '[String]' may fail; use 'as?' or 'as!' instead}}

  _ = (arr ?? []) as [String] // expected-warning {{coercion from '[Int]' to '[String]' may fail; use 'as?' or 'as!' instead}}
  // expected-warning@-1 {{left side of nil coalescing operator '??' has non-optional type '[Int]', so the right side is never used}}
  _ = (arr ?? [] ?? []) as [String] // expected-warning {{coercion from '[Int]' to '[String]' may fail; use 'as?' or 'as!' instead}}
  // expected-warning@-1 2{{left side of nil coalescing operator '??' has non-optional type '[Int]', so the right side is never used}}
  _ = (optArr ?? []) as [String] // expected-warning {{coercion from '[Int]' to '[String]' may fail; use 'as?' or 'as!' instead}}

  // Allow the coercion to increase optionality.
  _ = (arr ?? []) as [String]? // expected-warning {{coercion from '[Int]' to '[String]?' may fail; use 'as?' or 'as!' instead}}
  // expected-warning@-1 {{left side of nil coalescing operator '??' has non-optional type '[Int]', so the right side is never used}}
  _ = (arr ?? []) as [String?]? // expected-warning {{coercion from '[Int]' to '[String?]?' may fail; use 'as?' or 'as!' instead}}
  // expected-warning@-1 {{left side of nil coalescing operator '??' has non-optional type '[Int]', so the right side is never used}}
  _ = (arr ?? []) as [String??]?? // expected-warning {{coercion from '[Int]' to '[String??]??' may fail; use 'as?' or 'as!' instead}}
  // expected-warning@-1 {{left side of nil coalescing operator '??' has non-optional type '[Int]', so the right side is never used}}
  _ = (dict ?? [:]) as [String: String?]? // expected-warning {{coercion from '[String : Int]' to '[String : String?]?' may fail; use 'as?' or 'as!' instead}}
  // expected-warning@-1 {{left side of nil coalescing operator '??' has non-optional type '[String : Int]', so the right side is never used}}
  _ = (set ?? []) as Set<String>?? // expected-warning {{coercion from 'Set<Int>' to 'Set<String>??' may fail; use 'as?' or 'as!' instead}}
  // expected-warning@-1 {{left side of nil coalescing operator '??' has non-optional type 'Set<Int>', so the right side is never used}}

  // Allow the coercion to decrease optionality.
  _ = ohno(ohno(ohno(arr))) as [String] // expected-warning {{coercion from '[Int]???' to '[String]' may fail; use 'as?' or 'as!' instead}}
  _ = ohno(ohno(ohno(arr))) as [Int] // expected-warning {{coercion from '[Int]???' to '[Int]' may fail; use 'as?' or 'as!' instead}}
  _ = ohno(ohno(ohno(Set<Int>()))) as Set<String> // expected-warning {{coercion from 'Set<Int>???' to 'Set<String>' may fail; use 'as?' or 'as!' instead}}
  _ = ohno(ohno(ohno(["": ""]))) as [Int: String] // expected-warning {{coercion from '[String : String]???' to '[Int : String]' may fail; use 'as?' or 'as!' instead}}
  _ = ohno(ohno(ohno(dict))) as [String: Int] // expected-warning {{coercion from '[String : Int]???' to '[String : Int]' may fail; use 'as?' or 'as!' instead}}

  // In this case the array literal can be inferred to be [String], so totally
  // valid.
  _ = ([] ?? []) as [String] // expected-warning {{left side of nil coalescing operator '??' has non-optional type '[String]', so the right side is never used}}
  _ = (([] as Optional) ?? []) as [String]

  // The array can also be inferred to be [Any].
  _ = ([] ?? []) as Array // expected-warning {{left side of nil coalescing operator '??' has non-optional type '[Any]', so the right side is never used}}
}

// SR-13088
protocol JSON { }
protocol JSONLeaf: JSON {}
extension Int: JSONLeaf { }
extension Array: JSON where Element: JSON { }

protocol SR13035Error: Error {}
class ChildError: SR13035Error {}

protocol AnyC {
  func foo()
}

protocol AnyEvent {}

protocol A {
  associatedtype C: AnyC
}

protocol EventA: A {
  associatedtype Event
}

typealias Container<Namespace>
  = (event: Namespace.Event, c: Namespace.C) where Namespace: EventA

enum ConcreteA: EventA {
  struct C: AnyC {
    func foo() {}
  }

  enum Event: AnyEvent {
    case test
  }
}

protocol ProtocolP1 {}
protocol ProtocolQ1 {}
typealias Composition = ProtocolP1 & ProtocolQ1

protocol ProtocolP {}
protocol ProtocolQ {}

class ConcreteP: ProtocolP {}
class ConcreteQ: ProtocolQ {}
class ConcretePQ: ProtocolP, ProtocolQ {}
class ConcreteCPQ: ConcreteP, ProtocolQ {}

class ConcreteP1: ProtocolP1 {}
class ConcretePQ1: ProtocolP1, ProtocolQ1 {}

class ConcretePPQ1: ProtocolP, ProtocolP1, ProtocolQ1 {}
class NotConforms {}
struct StructNotComforms {}
final class NotConformsFinal {}

func tests_SR13088_false_positive_always_fail_casts() {
  // SR-13081
  let x: JSON = [4] // [4]
  _ = x as? [Any] // Ok

  // SR-13035
  func SR13035<SomeError: SR13035Error>(_ child: Result<String, ChildError>, _: Result<String, SomeError>) {
    let _ = child as? Result<String, SomeError> // Ok
  }

  func SR13035_1<SomeError: SR13035Error, Child: ChildError>(_ child: Result<String, Child>, parent: Result<String, SomeError>) {
    _ = child as? Result<String, SomeError> // Ok
    _ = parent as? Result<String, Child> // OK
  }

  // SR-11434 and SR-12321
  func encodable(_ value: Encodable) {
    _ = value as! [String : Encodable] // Ok
    _ = value as? [String: Encodable] // Ok
  }   

  // SR-13025
  func coordinate(_ event: AnyEvent, from c: AnyC) {
    switch (event, c) {
    case let container as Container<ConcreteA>: // OK
      container.c.foo()
    default:
      break
    }
  }

  // SR-7187
  let a: [Any] = [String?.some("hello") as Any, String?.none as Any]
  let b: [AnyObject] = [String?.some("hello") as AnyObject, String?.none as AnyObject]

  _ = a is [String?] // Ok
  _ = a as? [String?] as Any // OK
  _ = b is [String?] // Ok
  _ = b as? [String?] as AnyObject // OK

  // SR-6192
  let items = [String]()
  let dict = [String: Any]()
  let set = Set<String>()

  _ = items is [Int] // Ok
  _ = items as? [Int] as Any // Ok
  _ = items as! [Int] // Ok

  _ = dict is [Int: Any] // Ok
  _ = dict as? [Int: Any] as Any // Ok
  _ = dict as! [Int: Any] as Any // Ok

  _ = set is Set<Int> // Ok
  _ = set as? Set<Int> as Any // Ok
  _ = set as! Set<Int> // Ok

}

// Protocol composition
func protocol_composition(_ c: ProtocolP & ProtocolQ, _ c1: ProtocolP & Composition) {
  _ = c as? ConcretePQ // Ok
  _ = c as? ConcreteCPQ // Ok
  _ = c as? ConcreteP // Ok
  _ = c as? NotConforms // Ok
  _ = c as? StructNotComforms // expected-warning {{cast from 'ProtocolP & ProtocolQ' to unrelated type 'StructNotComforms' always fails}}
  _ = c as? NotConformsFinal // expected-warning {{cast from 'ProtocolP & ProtocolQ' to unrelated type 'NotConformsFinal' always fails}}
  _ = c1 as? ConcreteP // Ok
  _ = c1 as? ConcreteP1 // OK
  _ = c1 as? ConcretePQ1 // OK
  _ = c1 as? ConcretePPQ1 // Ok
  _ = c1 as? NotConforms // Ok
  _ = c1 as? StructNotComforms // expected-warning {{cast from 'ProtocolP & Composition' (aka 'ProtocolP & ProtocolP1 & ProtocolQ1') to unrelated type 'StructNotComforms' always fails}}
  _ = c1 as? NotConformsFinal // expected-warning {{cast from 'ProtocolP & Composition' (aka 'ProtocolP & ProtocolP1 & ProtocolQ1') to unrelated type 'NotConformsFinal' always fails}}
}
