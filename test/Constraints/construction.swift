// RUN: %target-parse-verify-swift

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

enum Optional<T> {
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

func acceptZ(z: Z) {}
func acceptString(s: String) {}

Point(1, 2)
var db : Base = d
X(i: 1, j: 2)
Y(1, 2, "hello")

// Unions
Z(UnicodeScalar("a"))
Z(1, 2)

acceptZ(.none)
acceptZ(.char("a"))
acceptString("\(hello), \(world) #\(i)!")

Optional<Int>(1)
Optional(1)
.none as Optional<Int>
Optional(.none) // expected-error{{could not find member 'none'}}

// Interpolation
"\(hello), \(world) #\(i)!"

class File {
  init() { 
    fd = 0
    body = ""
  }

  var fd : Int32, body : String
    func replPrint() {
    print("File{\n  fd=\(fd)\n  body=\"\(body)\"\n}")
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
b as! Derived

// Construction doesn't permit conversion.
// NOTE: Int and other integer-literal convertible types
//  are special cased in the library.
Int(i) // no-warning
i as Int
Z(z) // expected-error{{cannot find an initializer for type 'Z' that accepts an argument list of type '(Z)'}}
z as Z

// Construction from inouts.
struct FooRef { }
struct BarRef { 
  init(inout x: FooRef) {} 
  init(inout x: Int) {} 
}
var f = FooRef()
var x = 0
BarRef(x: &f)
BarRef(x: &x)

// Construction from a Type value not immediately resolved.
struct S1 {
  init(i: Int) { }
}

struct S2 {
  init(i: Int) { }
}

func getMetatype(i: Int) -> S1.Type { return S1.self }
func getMetatype(d: Double) -> S2.Type { return S2.self }

var s1 = getMetatype(1)(i: 5)
s1 = S1(i: 5)

var s2 = getMetatype(3.14)(i: 5)
s2 = S2(i: 5)

// rdar://problem/19254404
let i32 = Int32(123123123)
Int(i32 - 2 + 1)
