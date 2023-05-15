// RUN: %target-typecheck-verify-swift

final class DictStringInt : ExpressibleByDictionaryLiteral {
  typealias Key = String
  typealias Value = Int
  init(dictionaryLiteral elements: (String, Int)...) { }
}

final class MyDictionary<K, V> : ExpressibleByDictionaryLiteral {
  typealias Key = K
  typealias Value = V
  init(dictionaryLiteral elements: (K, V)...) { }
}

func useDictStringInt(_ d: DictStringInt) {}
func useDict<K, V>(_ d: MyDictionary<K,V>) {}

// Concrete dictionary literals.
useDictStringInt(["Hello" : 1])
useDictStringInt(["Hello" : 1, "World" : 2])
useDictStringInt(["Hello" : 1, "World" : 2.5])
// expected-error@-1 {{cannot convert value of type 'Double' to expected dictionary value type 'DictStringInt.Value' (aka 'Int')}}
useDictStringInt([4.5 : 2])
// expected-error@-1 {{cannot convert value of type 'Double' to expected dictionary key type 'DictStringInt.Key' (aka 'String')}}
useDictStringInt([nil : 2])
// expected-error@-1 {{'nil' is not compatible with expected dictionary key type 'DictStringInt.Key' (aka 'String')}}
useDictStringInt([7 : 1, "World" : 2])
// expected-error@-1 {{cannot convert value of type 'Int' to expected dictionary key type 'DictStringInt.Key' (aka 'String')}}
useDictStringInt(["Hello" : nil])
// expected-error@-1 {{'nil' is not compatible with expected dictionary value type 'DictStringInt.Value' (aka 'Int')}}

typealias FuncBoolToInt = (Bool) -> Int
let dict1: MyDictionary<String, FuncBoolToInt> = ["Hello": nil]
// expected-error@-1 {{'nil' is not compatible with expected dictionary value type 'MyDictionary<String, FuncBoolToInt>.Value' (aka '(Bool) -> Int')}}

// Generic dictionary literals.
useDict(["Hello" : 1])
useDict(["Hello" : 1, "World" : 2])
useDict(["Hello" : 1.5, "World" : 2])
useDict([1 : 1.5, 3 : 2.5])

// Fall back to Swift.Dictionary<K, V> if no context is otherwise available.
var a = ["Hello" : 1, "World" : 2]
var a2 : Dictionary<String, Int> = a
var a3 = ["Hello" : 1]

var b = [1 : 2, 1.5 : 2.5]
var b2 : Dictionary<Double, Double> = b
var b3 = [1 : 2.5]


// <rdar://problem/22584076> QoI: Using array literal init with dictionary produces bogus error

// expected-note @+1 {{did you mean to use a dictionary literal instead?}}
var _: MyDictionary<String, (Int) -> Int>? = [  // expected-error {{dictionary of type 'MyDictionary<String, (Int) -> Int>' cannot be initialized with array literal}}
  "closure_1" as String, {(Int) -> Int in 0},
  "closure_2", {(Int) -> Int in 0}]


var _: MyDictionary<String, Int>? = ["foo", 1]  // expected-error {{dictionary of type 'MyDictionary<String, Int>' cannot be initialized with array literal}}
// expected-note @-1 {{did you mean to use a dictionary literal instead?}} {{43-44=:}}

var _: MyDictionary<String, Int>? = ["foo", 1, "bar", 42]  // expected-error {{dictionary of type 'MyDictionary<String, Int>' cannot be initialized with array literal}}
// expected-note @-1 {{did you mean to use a dictionary literal instead?}} {{43-44=:}} {{53-54=:}}

var _: MyDictionary<String, Int>? = ["foo", 1.0, 2]  // expected-error {{dictionary of type 'MyDictionary<String, Int>' cannot be initialized with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{none}}

var _: MyDictionary<String, Int>? = ["foo" : 1.0]  // expected-error {{cannot convert value of type 'Double' to expected dictionary value type 'MyDictionary<String, Int>.Value' (aka 'Int')}}


// <rdar://problem/24058895> QoI: Should handle [] in dictionary contexts better
var _: [Int: Int] = []  // expected-error {{use [:] to get an empty dictionary literal}} {{22-22=:}}
var _ = useDictStringInt([]) // expected-error {{use [:] to get an empty dictionary literal}} {{27-27=:}}
var _: [[Int: Int]] = [[]] // expected-error {{use [:] to get an empty dictionary literal}} {{25-25=:}}
var _: [[Int: Int]?] = [[]] // expected-error {{use [:] to get an empty dictionary literal}} {{26-26=:}}
var assignDict = [1: 2]
assignDict = [] // expected-error {{use [:] to get an empty dictionary literal}} {{15-15=:}}

var _: [Int: Int] = [1] // expected-error {{dictionary of type '[Int : Int]' cannot be initialized with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{23-23=: <#value#>}}

var _: [Float: Int] = [1] // expected-error {{dictionary of type '[Float : Int]' cannot be initialized with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{25-25=: <#value#>}}

var _: [Int: Int] = ["foo"] // expected-error {{dictionary of type '[Int : Int]' cannot be initialized with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{27-27=: <#value#>}}

var _ = useDictStringInt(["Key"]) // expected-error {{dictionary of type 'DictStringInt' cannot be used with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{32-32=: <#value#>}}

var _ = useDictStringInt([4]) // expected-error {{dictionary of type 'DictStringInt' cannot be used with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{28-28=: <#value#>}}

var _: [[Int: Int]] = [[5]] // expected-error {{dictionary of type '[Int : Int]' cannot be used with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{26-26=: <#value#>}}

var _: [[Int: Int]] = [["bar"]] // expected-error {{dictionary of type '[Int : Int]' cannot be used with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{30-30=: <#value#>}}

assignDict = [1] // expected-error {{dictionary of type '[Int : Int]' cannot be used with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{16-16=: <#value#>}}

assignDict = [""] // expected-error {{dictionary of type '[Int : Int]' cannot be used with array literal}}
// expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{17-17=: <#value#>}}

func arrayLiteralDictionaryMismatch<T>(a: inout T) where T: ExpressibleByDictionaryLiteral, T.Key == Int, T.Value == Int {
  a = [] // expected-error {{use [:] to get an empty dictionary literal}} {{8-8=:}}

  a = [1] // expected-error {{dictionary of type 'T' cannot be used with array literal}}
  // expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{9-9=: <#value#>}}

  a = [""] // expected-error {{dictionary of type 'T' cannot be used with array literal}}
  // expected-note@-1 {{did you mean to use a dictionary literal instead?}} {{10-10=: <#value#>}}
}


class A { }
class B : A { }
class C : A { }

func testDefaultExistentials() {
  let _ = ["a": ["b": ["c": ["d", 1, true]]]]

  let _ = ["a" : 1, "b" : 2.5, "c" : "hello"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[String : Any]'; add explicit type annotation if this is intentional}}{{46-46= as [String : Any]}}

  let _: [String : Any] = ["a" : 1, "b" : 2.5, "c" : "hello"]

  let _ = ["a" : 1, "b" : nil, "c" : "hello"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[String : Any?]'; add explicit type annotation if this is intentional}}{{46-46= as [String : Any?]}}

  let _: [String : Any?] = ["a" : 1, "b" : nil, "c" : "hello"]

  let d2 = [:]
  // expected-error@-1{{empty collection literal requires an explicit type}}

  let _: Int = d2 // expected-error{{value of type '[AnyHashable : Any]'}}

  let _ = ["a": 1,
           "b": ["a", 2, 3.14159],
           "c": ["a": 2, "b": 3.5]]
  // expected-error@-3{{heterogeneous collection literal could only be inferred to '[String : Any]'; add explicit type annotation if this is intentional}}

  let d3 = ["b" : B(), "c" : C()]
  let _: Int = d3 // expected-error{{value of type '[String : A]'}}

  let _ = ["a" : B(), 17 : "seventeen", 3.14159 : "Pi"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[AnyHashable : Any]'}}

  let _ = ["a" : "hello", 17 : "string"]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[AnyHashable : String]'}}
}

/// rdar://problem/32330004
/// https://github.com/apple/swift/issues/47529
/// Assertion failure during `swift::ASTVisitor<::FailureDiagnosis,...>::visit`
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

// https://github.com/apple/swift/issues/59215
class S59215 {
  var m: [String: [String: String]] = [:]
  init() {
    m["a"] = ["", 1] //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
    // expected-note@-1{{did you mean to use a dictionary literal instead?}} {{17-18=:}}
    m["a"] = [1 , ""] //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
    // expected-note@-1{{did you mean to use a dictionary literal instead?}} {{17-18=:}}
    m["a"] = ["", ""] //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
    // expected-note@-1{{did you mean to use a dictionary literal instead?}} {{17-18=:}}
    m["a"] = [1 , 1] //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
    // expected-note@-1{{did you mean to use a dictionary literal instead?}} {{17-18=:}}
    m["a"] = Optional(["", ""]) //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
    // expected-note@-1{{did you mean to use a dictionary literal instead?}} {{26-27=:}}
  }
}

func f59215(_ a: [String: String]) {}
f59215(["", 1]) //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
// expected-note@-1{{did you mean to use a dictionary literal instead?}} {{11-12=:}}
f59215([1 , ""]) //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
// expected-note@-1{{did you mean to use a dictionary literal instead?}} {{11-12=:}}
f59215([1 , 1]) //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
// expected-note@-1{{did you mean to use a dictionary literal instead?}} {{11-12=:}}
f59215(["", ""]) //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
// expected-note@-1{{did you mean to use a dictionary literal instead?}} {{11-12=:}}

f59215(["", "", "", ""]) //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
// expected-note@-1{{did you mean to use a dictionary literal instead?}} {{11-12=:}} {{19-20=:}}
f59215(["", "", "", ""]) //expected-error{{dictionary of type '[String : String]' cannot be used with array literal}}
// expected-note@-1{{did you mean to use a dictionary literal instead?}} {{11-12=:}} {{19-20=:}}
