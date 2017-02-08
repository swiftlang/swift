// RUN: %target-typecheck-verify-swift

struct X {
  var i : Int, j : Int
}

struct Y {
  init (_ x : Int, _ y : Float, _ z : String) {}
}

enum Z {
  case none
  case char(UnicodeScalar)
  case string(String)
  case point(Int, Int)

  init() { self = .none }
  init(_ c: UnicodeScalar) { self = .char(c) }
  init(_ s: String) { self = .string(s) }
  init(_ x: Int, _ y: Int) { self = .point(x, y) }
}

enum Optional<T> {  // expected-note {{'T' declared as parameter to type 'Optional'}}
  case none
  case value(T)

  init() { self = .none }
  init(_ t: T) { self = .value(t) }
}

class Base { }
class Derived : Base { }

var d : Derived
typealias Point = (x : Int, y : Int)
var hello : String = "hello";
var world : String = "world";
var i : Int
var z : Z = .none

func acceptZ(_ z: Z) {}
func acceptString(_ s: String) {}

Point(1, 2) // expected-warning {{expression of type '(x: Int, y: Int)' is unused}}
var db : Base = d
X(i: 1, j: 2) // expected-warning{{unused}}
Y(1, 2, "hello") // expected-warning{{unused}}

// Unions
Z(UnicodeScalar("a")) // expected-warning{{unused}}
Z(1, 2) // expected-warning{{unused}}

acceptZ(.none)
acceptZ(.char("a"))
acceptString("\(hello), \(world) #\(i)!")

Optional<Int>(1) // expected-warning{{unused}}
Optional(1) // expected-warning{{unused}}
_ = .none as Optional<Int>
Optional(.none) // expected-error{{generic parameter 'T' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}} {{9-9=<Any>}}

// Interpolation
_ = "\(hello), \(world) #\(i)!"

class File {
  init() { 
    fd = 0
    body = ""
  }

  var fd : Int32, body : String
    func replPrint() {
    print("File{\n  fd=\(fd)\n  body=\"\(body)\"\n}", terminator: "")
  }
}

// Non-trivial references to metatypes.
struct Foo { 
  struct Inner { }
}
extension Foo {
  func getInner() -> Inner {
    return Inner()
  }
}

// Downcasting
var b : Base
_ = b as! Derived

// Construction doesn't permit conversion.
// NOTE: Int and other integer-literal convertible types
//  are special cased in the library.
Int(i) // expected-warning{{unused}}
_ = i as Int
Z(z) // expected-error{{cannot invoke initializer for type 'Z' with an argument list of type '(Z)'}}
// expected-note @-1 {{overloads for 'Z' exist with these partially matching parameter lists: (UnicodeScalar), (String)}}

Z.init(z)  // expected-error {{cannot invoke 'Z.Type.init' with an argument list of type '(Z)'}}
// expected-note @-1 {{overloads for 'Z.Type.init' exist with these partially matching parameter lists: (UnicodeScalar), (String)}}


_ = z as Z

// Construction from inouts.
struct FooRef { }
struct BarRef {
  init(x: inout FooRef) {}
  init(x: inout Int) {}
}
var f = FooRef()
var x = 0
BarRef(x: &f) // expected-warning{{unused}}
BarRef(x: &x) // expected-warning{{unused}}

// Construction from a Type value not immediately resolved.
struct S1 {
  init(i: Int) { }
}

struct S2 {
  init(i: Int) { }
}

func getMetatype(_ i: Int) -> S1.Type { return S1.self }
func getMetatype(_ d: Double) -> S2.Type { return S2.self }

var s1 = getMetatype(1).init(i: 5)
s1 = S1(i: 5)

var s2 = getMetatype(3.14).init(i: 5)
s2 = S2(i: 5)

// rdar://problem/19254404
let i32 = Int32(123123123)
Int(i32 - 2 + 1) // expected-warning{{unused}}

// rdar://problem/19459079
let xx: UInt64 = 100
let yy = ((xx + 10) - 5) / 5
let zy = (xx + (10 - 5)) / 5
