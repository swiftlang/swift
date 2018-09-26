// RUN: %target-typecheck-verify-swift

final class DictStringInt : ExpressibleByDictionaryLiteral {
  typealias Key = String
  typealias Value = Int
  init(dictionaryLiteral elements: (String, Int)...) { }
}

final class Dictionary<K, V> : ExpressibleByDictionaryLiteral {
  typealias Key = K
  typealias Value = V
  init(dictionaryLiteral elements: (K, V)...) { }
}

func useDictStringInt(_ d: DictStringInt) {}
func useDict<K, V>(_ d: Dictionary<K,V>) {}

// Concrete dictionary literals.
useDictStringInt(["Hello" : 1])
useDictStringInt(["Hello" : 1, "World" : 2])
useDictStringInt(["Hello" : 1, "World" : 2.5])
// expected-error@-1 {{cannot convert value of type 'Double' to expected dictionary value type 'Int'}}
useDictStringInt([4.5 : 2])
// expected-error@-1 {{cannot convert value of type 'Double' to expected dictionary key type 'String'}}
useDictStringInt([nil : 2])
// expected-error@-1 {{'nil' is not compatible with expected dictionary key type 'String'}}
useDictStringInt([7 : 1, "World" : 2])
// expected-error@-1 {{cannot convert value of type 'Int' to expected dictionary key type 'String'}}
useDictStringInt(["Hello" : nil])
// expected-error@-1 {{'nil' is not compatible with expected dictionary value type 'Int'}}

typealias FuncBoolToInt = (Bool) -> Int
let dict1: Dictionary<String, FuncBoolToInt> = ["Hello": nil]
// expected-error@-1 {{'nil' is not compatible with expected dictionary value type '(Bool) -> Int'}}

// Generic dictionary literals.
useDict(["Hello" : 1])
useDict(["Hello" : 1, "World" : 2])
useDict(["Hello" : 1.5, "World" : 2])
useDict([1 : 1.5, 3 : 2.5])

// Fall back to Dictionary<K, V> if no context is otherwise available.
var a = ["Hello" : 1, "World" : 2]
var a2 : Dictionary<String, Int> = a
var a3 = ["Hello" : 1]

var b = [1 : 2, 1.5 : 2.5]
var b2 : Dictionary<Double, Double> = b
var b3 = [1 : 2.5]


// <rdar://problem/22584076> QoI: Using array literal init with dictionary produces bogus error

// expected-note @+1 {{did you mean to use a dictionary literal instead?}}
var _: Dictionary<String, (Int) -> Int>? = [  // expected-error {{dictionary of type 'Dictionary<String, (Int) -> Int>' cannot be initialized with array literal}}
  "closure_1" as String, {(Int) -> Int in 0},
  "closure_2", {(Int) -> Int in 0}]


var _: Dictionary<String, Int>? = ["foo", 1]  // expected-error {{dictionary of type 'Dictionary<String, Int>' cannot be initialized with array literal}}
// expected-note @-1 {{did you mean to use a dictionary literal instead?}} {{41-42=:}}

var _: Dictionary<String, Int>? = ["foo", 1, "bar", 42]  // expected-error {{dictionary of type 'Dictionary<String, Int>' cannot be initialized with array literal}}
// expected-note @-1 {{did you mean to use a dictionary literal instead?}} {{41-42=:}} {{51-52=:}}

var _: Dictionary<String, Int>? = ["foo", 1.0, 2]  // expected-error {{cannot convert value of type '[Any]' to specified type 'Dictionary<String, Int>?'}}

var _: Dictionary<String, Int>? = ["foo" : 1.0]  // expected-error {{cannot convert value of type 'Double' to expected dictionary value type 'Int'}}


// <rdar://problem/24058895> QoI: Should handle [] in dictionary contexts better
var _: [Int: Int] = []  // expected-error {{use [:] to get an empty dictionary literal}} {{22-22=:}}


class A { }
class B : A { }
class C : A { }

func testDefaultExistentials() {
  let _ = ["a" : 1, "b" : 2.5, "c" : "hello"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to 'Dictionary<String, Any>'; add explicit type annotation if this is intentional}}{{46-46= as Dictionary<String, Any>}}

  let _: [String : Any] = ["a" : 1, "b" : 2.5, "c" : "hello"]

  let _ = ["a" : 1, "b" : nil, "c" : "hello"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to 'Dictionary<String, Any?>'; add explicit type annotation if this is intentional}}{{46-46= as Dictionary<String, Any?>}}

  let _: [String : Any?] = ["a" : 1, "b" : nil, "c" : "hello"]

  let d2 = [:]
  // expected-error@-1{{empty collection literal requires an explicit type}}

  let _: Int = d2 // expected-error{{value of type 'Dictionary<AnyHashable, Any>'}}

  let _ = ["a": 1,
           "b": ["a", 2, 3.14159],
           "c": ["a": 2, "b": 3.5]]
  // expected-error@-3{{heterogeneous collection literal could only be inferred to 'Dictionary<String, Any>'; add explicit type annotation if this is intentional}}

  let d3 = ["b" : B(), "c" : C()]
  let _: Int = d3 // expected-error{{value of type 'Dictionary<String, A>'}}

  let _ = ["a" : B(), 17 : "seventeen", 3.14159 : "Pi"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to 'Dictionary<AnyHashable, Any>'}}

  let _ = ["a" : "hello", 17 : "string"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to 'Dictionary<AnyHashable, String>'}}
}

// SR-4952, rdar://problem/32330004 - Assertion failure during swift::ASTVisitor<::FailureDiagnosis,...>::visit
func rdar32330004_1() -> [String: Any] {
  return ["a""one": 1, "two": 2, "three": 3] // expected-note {{did you mean to use a dictionary literal instead?}}
  // expected-error@-1 {{expected ',' separator}}
  // expected-error@-2 {{dictionary of type '[String : Any]' cannot be used with array literal}}
}

func rdar32330004_2() -> [String: Any] {
  return ["a", 0, "one", 1, "two", 2, "three", 3]
  // expected-error@-1 {{dictionary of type '[String : Any]' cannot be used with array literal}}
  // expected-note@-2 {{did you mean to use a dictionary literal instead?}} {{14-15=:}} {{24-25=:}} {{34-35=:}} {{46-47=:}}
}
