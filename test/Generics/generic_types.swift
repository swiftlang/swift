// RUN: %target-parse-verify-swift

protocol MyFormattedPrintable {
  func myFormat() -> String
}

func myPrintf(format: String, args: MyFormattedPrintable...) {}

extension Int : MyFormattedPrintable {
  func myFormat() -> String { return "" }
}

struct S<T : MyFormattedPrintable> {
  var c : T
  static func f(a: T) -> T {
    return a
  }
  func f(a: T, b: Int) {
    return myPrintf("%v %v %v", a, b, c)
  }
}

func makeSInt() -> S<Int> {}

typealias SInt = S<Int>
var a : S<Int> = makeSInt()
a.f(1,b: 2)
var b : Int = SInt.f(1)

struct S2<T> {
  static func f() -> T {
    S2.f()
  }
}

struct X { }

var d : S<X> // expected-error{{type 'X' does not conform to protocol 'MyFormattedPrintable'}}

enum Optional<T> {
  case Element(T)
  case None

  init() { self = .None }
  init(_ t: T) { self = .Element(t) }
}
typealias OptionalInt = Optional<Int>
var uniontest1 : (Int) -> Optional<Int> = OptionalInt.Element // expected-error{{partial application of enum constructor}}
var uniontest2 : Optional<Int> = OptionalInt.None
var uniontest3 = OptionalInt(1)

// FIXME: Stuff that should work, but doesn't yet.
// var uniontest4 : OptInt = .None
// var uniontest5 : OptInt = .Some(1)

func formattedTest<T : MyFormattedPrintable>(a: T) {
  myPrintf("%v", a)
}
struct formattedTestS<T : MyFormattedPrintable> {
  func f(a: T) {
    formattedTest(a)
  }
}

struct GenericReq<
  T : GeneratorType, U : GeneratorType where T.Element == U.Element
> {}

func getFirst<R : GeneratorType>(var r: R) -> R.Element {
  return r.next()!
}

func testGetFirst(var ir: Range<Int>) {
  var i : Int = getFirst(ir.generate())
}

struct XT<T> {
  init(t : T) {
    prop = (t, t)
  }

  static func f() -> T {}
  func g() -> T {}

  var prop : (T, T)
}

class YT<T> {
  init(_ t : T) {
    prop = (t, t)
  }
  deinit {}

  class func f() -> T {}
  func g() -> T {}

  var prop : (T, T)
}

struct ZT<T> {
  var x : T, f : Float
}

struct Dict<K, V> {
  subscript(key: K) -> V { get {} set {} }
}

class Dictionary<K, V> { // expected-note{{generic type 'Dictionary' declared here}}
  subscript(key: K) -> V { get {} set {} }
}

typealias XI = XT<Int>
typealias YI = YT<Int>
typealias ZI = ZT<Int>

var xi = XI(t: 17)
var yi = YI(17)
var zi = ZI(x: 1, f: 3.0)

var i : Int = XI.f()
i = XI.f()
i = xi.g()
i = yi.f() // expected-error{{does not have a member named 'f'}}
i = yi.g()

var xif : (XI) -> () -> Int = XI.g
var gif : (YI) -> () -> Int = YI.g

var ii : (Int, Int) = xi.prop
ii = yi.prop
xi.prop = ii
yi.prop = ii

var d1 : Dict<String, Int>
var d2 : Dictionary<String, Int>
d1["hello"] = d2["world"]
i = d2["blarg"]

struct RangeOfPrintables<R : SequenceType
         where R.Generator.Element : MyFormattedPrintable> {
  var r : R

  func format() -> String {
    var s : String
    for e in r {
      s = s + e.myFormat() + " "
    }
    return s
  }
}

struct Y {}
struct SequenceY : SequenceType, GeneratorType {
  typealias Generator = SequenceY
  typealias Element = Y

  func next() -> Element? { return Y() }
  func generate() -> Generator { return self }
}

func useRangeOfPrintables(roi : RangeOfPrintables<[Int]>) {
  var rop : RangeOfPrintables<X> // expected-error{{type 'X' does not conform to protocol 'SequenceType'}}
  var rox : RangeOfPrintables<SequenceY> // expected-error{{type 'Element' does not conform to protocol 'MyFormattedPrintable'}}
}

struct HasNested<T> {
  init<U>(_ t: T, _ u: U) {}
  func f<U>(t: T, u: U) -> (T, U) {}

  struct InnerGeneric<U> { // expected-error{{generic type 'InnerGeneric' nested}}
    init() {}
    func g<V>(t: T, u: U, v: V) -> (T, U, V) {}
  }

  struct Inner { // expected-error{{nested in generic type}}
    init (_ x: T) {}
    func identity(x: T) -> T { return x }
  }
}

func useNested(ii: Int, hni: HasNested<Int>,
               xisi : HasNested<Int>.InnerGeneric<String>,
               xfs: HasNested<Float>.InnerGeneric<String>) {
  var i = ii, xis = xisi
  typealias InnerI = HasNested<Int>.Inner
  var innerI = InnerI(5)
  typealias InnerF = HasNested<Float>.Inner
  var innerF : InnerF = innerI // expected-error{{'Int' is not identical to 'Float'}}

  innerI.identity(i)
  i = innerI.identity(i)

  // Generic function in a generic class
  typealias HNI = HasNested<Int>
  var id = hni.f(1, u: 3.14159)
  id = (2, 3.14159)
  hni.f(1.5, 3.14159) // expected-error{{cannot invoke 'f' with an argument list of type '(Double, Double)'}}

  // Generic constructor of a generic struct
  HNI(1, 2.71828)
  // FIXME: Should report this error: {{cannot convert the expression's type 'HNI' to type 'Int'}}
  HNI(1.5, 2.71828) // expected-error{{cannot invoke initializer for type 'HNI' with an argument list of type '(Double, Double)'}}

  // Generic function in a nested generic struct
  var ids = xis.g(1, u: "Hello", v: 3.14159)
  ids = (2, "world", 2.71828)

  xis = xfs // expected-error{{cannot assign a value of type 'HasNested<Float>.InnerGeneric<String>' to a value of type 'HasNested<Int>.InnerGeneric<String>'}}
}

var dfail : Dictionary<Int> // expected-error{{generic type 'Dictionary' specialized with too few type parameters (got 1, but expected 2)}}
var notgeneric : Int<Float> // expected-error{{cannot specialize non-generic type 'Int'}}

// Check unqualified lookup of inherited types.
class Foo<T> {
  typealias Nested = T
}

class Bar : Foo<Int> { // expected-error {{classes derived from generic classes must also be generic}}
  func f(x: Int) -> Nested {
    return x
  }

  struct Inner {
    func g(x: Int) -> Nested {
      return x
    }

    func withLocal() {
      struct Local {
        func h(x: Int) -> Nested {
          return x
        }
      }
    }
  }
}

extension Bar {
  func g(x: Int) -> Nested {
    return x
  }

  /* This crashes for unrelated reasons: <rdar://problem/14376418>
  struct Inner2 {
    func f(x: Int) -> Nested {
      return x
    }    
  }
  */
}

// Make sure that redundant typealiases (that map to the same
// underlying type) don't break protocol conformance or use.
class XArray : ArrayLiteralConvertible {
  typealias Element = Int
  init() { }

  required init(arrayLiteral elements: Int...) { }
}

class YArray : XArray, ArrayLiteralConvertible {
  typealias Element = Int
  required init(arrayLiteral elements: Int...) { 
    super.init()
  }
}

var yarray : YArray = [1, 2, 3]
var xarray : XArray = [1, 2, 3]

// Type parameters can be referenced only via unqualified name lookup
struct XParam<T> {
  func foo(x: T) {
    var y : T = x
  }

  static func bar(x: T) {
    var y : T = x
  }
}

var xp : XParam<Int>.T = Int() // expected-error{{'T' is not a member type of 'XParam<Int>'}}

// Diagnose failure to meet a superclass requirement.
class X1 { }
class X2<T : X1> { }
class X3 { }

var x2 : X2<X3> // expected-error{{type 'X3' does not inherit from 'X1'}}
