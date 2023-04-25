// RUN: %target-typecheck-verify-swift -disable-availability-checking

struct IntList : ExpressibleByArrayLiteral {
  typealias Element = Int
  init(arrayLiteral elements: Int...) {}
}

struct DoubleList : ExpressibleByArrayLiteral {
  typealias Element = Double
  init(arrayLiteral elements: Double...) {}
}

struct IntDict : ExpressibleByArrayLiteral {
  typealias Element = (String, Int)
  init(arrayLiteral elements: Element...) {}
}

final class DoubleDict : ExpressibleByArrayLiteral {
  typealias Element = (String, Double)
  init(arrayLiteral elements: Element...) {}
}

final class List<T> : ExpressibleByArrayLiteral {
  typealias Element = T
  init(arrayLiteral elements: T...) {}
}

final class Dict<K,V> : ExpressibleByArrayLiteral {
  typealias Element = (K,V)

  init(arrayLiteral elements: (K,V)...) {}
}

infix operator =>

func => <K, V>(k: K, v: V) -> (K,V) { return (k,v) }

func useIntList(_ l: IntList) {}
func useDoubleList(_ l: DoubleList) {}
func useIntDict(_ l: IntDict) {}
func useDoubleDict(_ l: DoubleDict) {}
func useList<T>(_ l: List<T>) {}
func useDict<K,V>(_ d: Dict<K,V>) {}

useIntList([1,2,3])
useIntList([1.0,2,3]) // expected-error{{cannot convert value of type 'Double' to expected element type 'Int'}}
useIntList([nil])  // expected-error {{'nil' is not compatible with expected element type 'Int'}}

useDoubleList([1.0,2,3])
useDoubleList([1.0,2.0,3.0])

useIntDict(["Niners" => 31, "Ravens" => 34])
useIntDict(["Niners" => 31, "Ravens" => 34.0]) // expected-error{{cannot convert value of type 'Double' to expected element type 'Int'}}
// <rdar://problem/22333090> QoI: Propagate contextual information in a call to operands
useDoubleDict(["Niners" => 31, "Ravens" => 34.0])
useDoubleDict(["Niners" => 31.0, "Ravens" => 34])
useDoubleDict(["Niners" => 31.0, "Ravens" => 34.0])

// Generic slices
useList([1,2,3])
useList([1.0,2,3])
useList([1.0,2.0,3.0])
useDict(["Niners" => 31, "Ravens" => 34])
useDict(["Niners" => 31, "Ravens" => 34.0])
useDict(["Niners" => 31.0, "Ravens" => 34.0])

// Fall back to [T] if no context is otherwise available.
var a = [1,2,3]
var a2 : [Int] = a

var b = [1,2,3.0]
var b2 : [Double] = b

var arrayOfStreams = [1..<2, 3..<4]

struct MyArray : ExpressibleByArrayLiteral {
  typealias Element = Double

  init(arrayLiteral elements: Double...) {
    
  }
}

var myArray : MyArray = [2.5, 2.5]

// Inference for tuple elements.
var x1 = [1]
x1[0] = 0
var x2 = [(1, 2)]
x2[0] = (3, 4)
var x3 = [1, 2, 3]
x3[0] = 4

func trailingComma() {
  _ = [1, ]
  _ = [1, 2, ]
  _ = ["a": 1, ]
  _ = ["a": 1, "b": 2, ]
}

func longArray() {
  var _=["1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1"]
}

[1,2].map // expected-error {{generic parameter 'T' could not be inferred}}


// <rdar://problem/25563498> Type checker crash assigning array literal to type conforming to ArrayProtocol
func rdar25563498<T : ExpressibleByArrayLiteral>(t: T) {
  var x: T = [1] // expected-error {{cannot convert value of type 'Int' to expected element type 'T.ArrayLiteralElement'}}
}

func rdar25563498_ok<T : ExpressibleByArrayLiteral>(t: T) -> T
     where T.ArrayLiteralElement : ExpressibleByIntegerLiteral {
  let x: T = [1]
  return x
}

class A { }
class B : A { }
class C : A { }

/// Check for defaulting the element type to 'Any' / 'Any?'.
func defaultToAny(i: Int, s: String) {
  let a1 = [1, "a", 3.5]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[Any]'; add explicit type annotation if this is intentional}}
  let _: Int = a1  // expected-error{{value of type '[Any]'}}

  let _ = ([1, "a"])
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[Any]'; add explicit type annotation if this is intentional}}
  let _ = [1, true, []]
  // expected-error@-1:11 {{heterogeneous collection literal could only be inferred to '[Any]'; add explicit type annotation if this is intentional}}

  let a2: Array = [1, "a", 3.5]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[Any]'; add explicit type annotation if this is intentional}}
  let _: Int = a2  // expected-error{{value of type '[Any]'}}
  
  let a3 = [1, "a", nil, 3.5]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[Any?]'; add explicit type annotation if this is intentional}}
  let _: Int = a3 // expected-error{{value of type '[Any?]'}}
  
  let a4: Array = [1, "a", nil, 3.5]
  // expected-error@-1{{heterogeneous collection literal could only be inferred to '[Any?]'; add explicit type annotation if this is intentional}}
  let _: Int = a4 // expected-error{{value of type '[Any?]'}}

  let a5 = []
  // expected-error@-1{{empty collection literal requires an explicit type}}
  let _: Int = a5 // expected-error{{value of type '[Any]'}}

  let _: [Any] = []
  let _: [Any] = [1, "a", 3.5]
  let _: [Any] = [1, "a", [3.5, 3.7, 3.9]]
  let _: [Any] = [1, "a", [3.5, "b", 3]]
  let _: [Any] = [1, [2, [3]]]

  func f1() -> [Any] {
    []
  }
  
  let _: [Any?] = [1, "a", nil, 3.5]
  let _: [Any?] = [1, "a", nil, [3.5, 3.7, 3.9]]
  let _: [Any?] = [1, "a", nil, [3.5, "b", nil]]
  let _: [Any?] = [1, [2, [3]]]
  let _: [Any?] = [1, nil, [2, nil, [3]]]

  let a6 = [B(), C()]
  let _: Int = a6 // expected-error{{value of type '[A]'}}
  
  let a7: some Collection = [1, "Swift"]
  let _: (any Sequence)? = [1, "Swift"]
  let _: any Sequence = [1, nil, "Swift"]
  let _ = true ? [] : []
  let _ = (true, ([1, "Swift"]))

  func f2<T>(_: [T]) {}

  func f3<T>() -> [T]? {}

  f2([])
  f2([1, nil, ""])
  _ = f3() ?? []
}

func noInferAny(iob: inout B, ioc: inout C) {
  var b = B()
  var c = C()
  let _ = [b, c, iob, ioc] // do not infer [Any] when elements are lvalues or inout
  let _: [A] = [b, c, iob, ioc] // do not infer [Any] when elements are lvalues or inout
  b = B()
  c = C()
}

/// Check handling of 'nil'.
protocol Proto1 {}
protocol Proto2 {}
struct Nilable: ExpressibleByNilLiteral {
	init(nilLiteral: ()) {}
}
func joinWithNil<T>(s: String, a: Any, t: T, m: T.Type, p: Proto1 & Proto2, arr: [Int], opt: Int?, iou: Int!, n: Nilable) {
  let a1 = [s, nil]
  let _: Int = a1 // expected-error{{value of type '[String?]'}}

  let a2 = [nil, s]
  let _: Int = a2 // expected-error{{value of type '[String?]'}}

  let a3 = ["hello", nil]
  let _: Int = a3 // expected-error{{value of type '[String?]'}}

  let a4 = [nil, "hello"]
  let _: Int = a4 // expected-error{{value of type '[String?]'}}
  
  let a5 = [(s, s), nil]
  let _: Int = a5 // expected-error{{value of type '[(String, String)?]'}}
  
  let a6 = [nil, (s, s)]
  let _: Int = a6 // expected-error{{value of type '[(String, String)?]'}}
  
  let a7 = [("hello", "world"), nil]
  let _: Int = a7 // expected-error{{value of type '[(String, String)?]'}}
  
  let a8 = [nil, ("hello", "world")]
  let _: Int = a8 // expected-error{{value of type '[(String, String)?]'}}
  
  let a9 = [{ $0 * 2 }, nil]
  let _: Int = a9 // expected-error{{value of type '[((Int) -> Int)?]'}}
  
  let a10 = [nil, { $0 * 2 }]
  let _: Int = a10 // expected-error{{value of type '[((Int) -> Int)?]'}}
  
  let a11 = [a, nil]
  let _: Int = a11 // expected-error{{value of type '[Any?]'}}
  
  let a12 = [nil, a]
  let _: Int = a12 // expected-error{{value of type '[Any?]'}}
  
  let a13 = [t, nil]
  let _: Int = a13 // expected-error{{value of type '[T?]'}}
  
  let a14 = [nil, t]
  let _: Int = a14 // expected-error{{value of type '[T?]'}}
  
  let a15 = [m, nil]
  let _: Int = a15 // expected-error{{value of type '[T.Type?]'}}
  
  let a16 = [nil, m]
  let _: Int = a16 // expected-error{{value of type '[T.Type?]'}}
  
  let a17 = [p, nil]
  let _: Int = a17 // expected-error{{value of type '[(any Proto1 & Proto2)?]'}}
  
  let a18 = [nil, p]
  let _: Int = a18 // expected-error{{value of type '[(any Proto1 & Proto2)?]'}}
  
  let a19 = [arr, nil]
  let _: Int = a19 // expected-error{{value of type '[[Int]?]'}}
  
  let a20 = [nil, arr]
  let _: Int = a20 // expected-error{{value of type '[[Int]?]'}}
  
  let a21 = [opt, nil]
  let _: Int = a21 // expected-error{{value of type '[Int?]'}}
  
  let a22 = [nil, opt]
  let _: Int = a22 // expected-error{{value of type '[Int?]'}}
  
  let a23 = [iou, nil]
  let _: Int = a23 // expected-error{{value of type '[Int?]'}}
  
  let a24 = [nil, iou]
  let _: Int = a24 // expected-error{{value of type '[Int?]'}}
  
  let a25 = [n, nil]
  let _: Int = a25 // expected-error{{value of type '[Nilable]'}}
  
  let a26 = [nil, n]
  let _: Int = a26 // expected-error{{value of type '[Nilable]'}}
}

struct OptionSetLike : ExpressibleByArrayLiteral {
  typealias Element = OptionSetLike
  init() { }

  init(arrayLiteral elements: OptionSetLike...) { }

  static let option: OptionSetLike = OptionSetLike()
}

func testOptionSetLike(b: Bool) {
  let _: OptionSetLike = [ b ? [] : OptionSetLike.option, OptionSetLike.option]
  let _: OptionSetLike = [ b ? [] : .option, .option]
}

// Join of class metatypes - <rdar://problem/30233451>

class Company<T> {
  init(routes: [() -> T]) { }
}

class Person { }

class Employee: Person { }

class Manager: Person { }

let routerPeople = Company(
  routes: [
    { () -> Employee.Type in
      _ = ()
      return Employee.self
    },

    { () -> Manager.Type in
      _ = ()
      return Manager.self
    }
  ]
)

// Same as above but with existentials

protocol Fruit {}

protocol Tomato : Fruit {}

struct Chicken : Tomato {}

protocol Pear : Fruit {}

struct Beef : Pear {}

let routerFruit = Company(
  routes: [
    { () -> Tomato.Type in
      _ = ()
      return Chicken.self
    },

    { () -> Pear.Type in
      _ = ()
      return Beef.self
    }
  ]
)

// https://github.com/apple/swift/issues/46371
do {
  let x: [Int] = [1, 2, 3]

  // Infer '[[Int]]'.
  // FIXME: As noted in the issue, this was the behavior in Swift 3, but
  // it seems like the wrong choice and is less by design than by accident.
  let _ = [x.reversed(), x]
}

// Conditional conformance
protocol P { }

struct PArray<T> { }

extension PArray : ExpressibleByArrayLiteral where T: P {
  typealias ArrayLiteralElement = T

  init(arrayLiteral elements: T...) { }
}

extension Int: P { }

func testConditional(i: Int, s: String) {
  let _: PArray<Int> = [i, i, i]
  let _: PArray<String> = [s, s, s] // expected-error{{cannot convert value of type '[String]' to specified type 'PArray<String>'}}
}


// https://github.com/apple/swift/issues/50912
do {
  enum Enum: ExpressibleByStringLiteral {
    case text(String)
    init(stringLiteral value: String) {
      self = .text(value)
    }
  }

  let _: [Enum] = [Enum("hello")]
  let _: [Enum] = [.text("hello")]
  let _: [Enum] = ["hello", Enum.text("world")]
  let _: [Enum] = ["hello", .text("world")]
}

struct TestMultipleOverloadedInits {
  var x: Double
  func foo() {
    let _ = [Float(x), Float(x), Float(x), Float(x)]
  }
}
