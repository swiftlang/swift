// RUN: %target-typecheck-verify-swift

protocol MyFormattedPrintable { // expected-note 3 {{declared here}}
  func myFormat() -> String
}

func myPrintf(_ format: String, _ args: MyFormattedPrintable...) {}

extension Int : MyFormattedPrintable {
  func myFormat() -> String { return "" }
}

struct S<T : MyFormattedPrintable> {
  var c : T
  static func f(_ a: T) -> T {
    return a
  }
  func f(_ a: T, b: Int) {
    return myPrintf("%v %v %v", a, b, c)
  }
}

func makeSInt() -> S<Int> {}

typealias SInt = S<Int>
var a : S<Int> = makeSInt()
a.f(1,b: 2)
var b : Int = SInt.f(1)

struct S2<T> {
  @discardableResult
  static func f() -> T {
    S2.f()
  }
}

struct X { }

var d : S<X> // expected-error{{type 'X' does not conform to protocol 'MyFormattedPrintable'}}

enum Optional<T> {
  case element(T)
  case none

  init() { self = .none }
  init(_ t: T) { self = .element(t) }
}
typealias OptionalInt = Optional<Int>
var uniontest1 : (Int) -> Optional<Int> = OptionalInt.element
var uniontest2 : Optional<Int> = OptionalInt.none
var uniontest3 = OptionalInt(1)

// FIXME: Stuff that should work, but doesn't yet.
// var uniontest4 : OptInt = .none
// var uniontest5 : OptInt = .Some(1)

func formattedTest<T : MyFormattedPrintable>(_ a: T) {
  myPrintf("%v", a)
}
struct formattedTestS<T : MyFormattedPrintable> {
  func f(_ a: T) {
    formattedTest(a)
  }
}

struct GenericReq<T : IteratorProtocol, U : IteratorProtocol>
  where T.Element == U.Element {
}

func getFirst<R : IteratorProtocol>(_ r: R) -> R.Element {
  var r = r
  return r.next()!
}

func testGetFirst(ir: Range<Int>) {
  _ = getFirst(ir.makeIterator()) as Int
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

class Dictionary<K, V> { // expected-note{{generic class 'Dictionary' declared here}}
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
i = yi.f() // expected-error{{static member 'f' cannot be used on instance of type 'YI' (aka 'YT<Int>')}}
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

struct RangeOfPrintables<R : Sequence>
  where R.Iterator.Element : MyFormattedPrintable {
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
struct SequenceY : Sequence, IteratorProtocol {
  typealias Iterator = SequenceY
  typealias Element = Y

  func next() -> Element? { return Y() }
  func makeIterator() -> Iterator { return self }
}

func useRangeOfPrintables(_ roi : RangeOfPrintables<[Int]>) {
  var rop : RangeOfPrintables<X> // expected-error{{type 'X' does not conform to protocol 'Sequence'}}
  var rox : RangeOfPrintables<SequenceY> // expected-error{{type 'SequenceY.Element' (aka 'Y') does not conform to protocol 'MyFormattedPrintable'}}
}

var dfail : Dictionary<Int> // expected-error{{generic type 'Dictionary' specialized with too few type parameters (got 1, but expected 2)}}
var notgeneric : Int<Float> // expected-error{{cannot specialize non-generic type 'Int'}}{{21-28=}}
var notgenericNested : Array<Int<Float>> // expected-error{{cannot specialize non-generic type 'Int'}}{{33-40=}}

// Make sure that redundant typealiases (that map to the same
// underlying type) don't break protocol conformance or use.
class XArray : ExpressibleByArrayLiteral {
  typealias Element = Int
  init() { }

  required init(arrayLiteral elements: Int...) { }
}

class YArray : XArray {
  typealias Element = Int
  required init(arrayLiteral elements: Int...) {
    super.init()
  }
}

var yarray : YArray = [1, 2, 3]
var xarray : XArray = [1, 2, 3]

// Type parameters can be referenced only via unqualified name lookup
struct XParam<T> { // expected-note{{'XParam' declared here}}
  func foo(_ x: T) {
    _ = x as T
  }

  static func bar(_ x: T) {
    _ = x as T
  }
}

var xp : XParam<Int>.T = Int() // expected-error{{'T' is not a member type of generic struct 'generic_types.XParam<Swift.Int>'}}

// Diagnose failure to meet a superclass requirement.
class X1 { }
class X2<T : X1> { } // expected-note{{requirement specified as 'T' : 'X1' [with T = X3]}}
class X3 { }

var x2 : X2<X3> // expected-error{{'X2' requires that 'X3' inherit from 'X1'}}

protocol P {
  associatedtype AssocP
}

protocol Q {
  associatedtype AssocQ
}

struct X4 : P, Q {
  typealias AssocP = Int
  typealias AssocQ = String
}

struct X5<T, U> where T: P, T: Q, T.AssocP == T.AssocQ { } // expected-note{{requirement specified as 'T.AssocP' == 'T.AssocQ' [with T = X4]}}

var y: X5<X4, Int> // expected-error{{'X5' requires the types 'X4.AssocP' (aka 'Int') and 'X4.AssocQ' (aka 'String') be equivalent}}

// Recursive generic signature validation.
class Top {}
class Bottom<T : Bottom<Top>> {}
// expected-error@-1 {{'Bottom' requires that 'Top' inherit from 'Bottom<Top>'}}
// expected-note@-2 {{requirement specified as 'T' : 'Bottom<Top>' [with T = Top]}}
// expected-error@-3 *{{generic class 'Bottom' has self-referential generic requirements}}

// Invalid inheritance clause

struct UnsolvableInheritance1<T : T.A> {}
// expected-error@-1 {{'A' is not a member type of type 'T'}}
// expected-error@-2 {{type 'T' constrained to non-protocol, non-class type 'T.A'}}

struct UnsolvableInheritance2<T : U.A, U : T.A> {}
// expected-error@-1 {{'A' is not a member type of type 'U'}}
// expected-error@-2 {{'A' is not a member type of type 'T'}}
// expected-error@-3 {{type 'T' constrained to non-protocol, non-class type 'U.A'}}
// expected-error@-4 {{type 'U' constrained to non-protocol, non-class type 'T.A'}}

enum X7<T> where X7.X : G { case X } // expected-error{{enum case 'X' is not a member type of 'X7<T>'}}
// expected-error@-1{{cannot find type 'G' in scope}}

// Test that contextual type resolution for generic metatypes is consistent
// under a same-type constraint.
protocol MetatypeTypeResolutionProto {}
struct X8<T> {
  static var property1: T.Type { T.self }
  static func method1() -> T.Type { T.self }
}
extension X8 where T == MetatypeTypeResolutionProto {
  static var property2: T.Type { property1 } // ok, still .Protocol
  static func method2() -> T.Type { method1() } // ok, still .Protocol
}

func bogusProtocolConstraint1(_ : any MyFormattedPrintable<String>) {}
// expected-error@-1 {{protocol 'MyFormattedPrintable' does not have primary associated types that can be constrained}}{{59-67=}}
func bogusProtocolConstraint2(_ : some MyFormattedPrintable<String>) {}
// expected-error@-1 {{protocol 'MyFormattedPrintable' does not have primary associated types that can be constrained}}{{60-68=}}
func bogusProtocolConstraint3(_ : MyFormattedPrintable<String>) {}
// expected-error@-1 {{protocol 'MyFormattedPrintable' does not have primary associated types that can be constrained}}{{55-63=}}
