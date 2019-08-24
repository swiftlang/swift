// RUN: %target-typecheck-verify-swift

func f0(_ x: inout Int) {}
func f1<T>(_ x: inout T) {}
func f2(_ x: inout X) {}
func f2(_ x: inout Double) {}

class Reftype {
  var property: Double { get {} set {} }
}

struct X {
  subscript(i: Int) -> Float { get {} set {} }

  var property: Double { get {} set {} }

  func genuflect() {}
}

struct Y {
  subscript(i: Int) -> Float { get {} set {} }
  subscript(f: Float) -> Int { get {} set {} }
}

var i : Int
var f : Float
var x : X
var y : Y

func +=(lhs: inout X, rhs : X) {}
prefix operator +++
prefix func +++(rhs: inout X) {}

f0(&i)
f1(&i)
f1(&x[i])
f1(&x.property)
f1(&y[i])

// Missing '&'
f0(i) // expected-error{{passing value of type 'Int' to an inout parameter requires explicit '&'}}{{4-4=&}}
f1(y[i]) // expected-error{{passing value of type 'Float' to an inout parameter requires explicit '&'}} {{4-4=&}}

// Assignment operators
x += x
+++x

var yi = y[i]

// Non-settable lvalues

var non_settable_x : X {
  return x
}

struct Z {
  var non_settable_x: X { get {} }
  var non_settable_reftype: Reftype { get {} }
  var settable_x : X

  subscript(i: Int) -> Double { get {} }
  subscript(_: (i: Int, j: Int)) -> X { get {} }
}

var z : Z

func fz() -> Z {}
func fref() -> Reftype {}

// non-settable var is non-settable:
// - assignment
non_settable_x = x // expected-error{{cannot assign to value: 'non_settable_x' is a get-only property}}
// - inout (mono)
f2(&non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
// - inout (generic)
f1(&non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
// - inout assignment
non_settable_x += x // expected-error{{left side of mutating operator isn't mutable: 'non_settable_x' is a get-only property}}
+++non_settable_x // expected-error{{cannot pass immutable value to mutating operator: 'non_settable_x' is a get-only property}}

// non-settable property is non-settable:
z.non_settable_x = x // expected-error{{cannot assign to property: 'non_settable_x' is a get-only property}}
f2(&z.non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
f1(&z.non_settable_x) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
z.non_settable_x += x // expected-error{{left side of mutating operator isn't mutable: 'non_settable_x' is a get-only property}}
+++z.non_settable_x // expected-error{{cannot pass immutable value to mutating operator: 'non_settable_x' is a get-only property}}

// non-settable subscript is non-settable:
z[0] = 0.0 // expected-error{{cannot assign through subscript: subscript is get-only}}
f2(&z[0]) // expected-error{{cannot pass immutable value as inout argument: subscript is get-only}}
f1(&z[0]) // expected-error{{cannot pass immutable value as inout argument: subscript is get-only}}
z[0] += 0.0 // expected-error{{left side of mutating operator isn't mutable: subscript is get-only}}
+++z[0] // expected-error{{cannot pass immutable value as inout argument: subscript is get-only}}

// settable property of an rvalue value type is non-settable:
fz().settable_x = x // expected-error{{cannot assign to property: 'fz' returns immutable value}}
f2(&fz().settable_x) // expected-error{{cannot pass immutable value as inout argument: 'fz' returns immutable value}}
f1(&fz().settable_x) // expected-error{{cannot pass immutable value as inout argument: 'fz' returns immutable value}}
fz().settable_x += x // expected-error{{left side of mutating operator isn't mutable: 'fz' returns immutable value}}
+++fz().settable_x // expected-error{{cannot pass immutable value to mutating operator: 'fz' returns immutable value}}

// settable property of an rvalue reference type IS SETTABLE:
fref().property = 0.0
f2(&fref().property)
f1(&fref().property)
fref().property += 0.0
fref().property += 1

// settable property of a non-settable value type is non-settable:
z.non_settable_x.property = 1.0 // expected-error{{cannot assign to property: 'non_settable_x' is a get-only property}}
f2(&z.non_settable_x.property) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
f1(&z.non_settable_x.property) // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}
z.non_settable_x.property += 1.0 // expected-error{{left side of mutating operator isn't mutable: 'non_settable_x' is a get-only property}}
+++z.non_settable_x.property // expected-error{{cannot pass immutable value as inout argument: 'non_settable_x' is a get-only property}}

// settable property of a non-settable reference type IS SETTABLE:
z.non_settable_reftype.property = 1.0
f2(&z.non_settable_reftype.property)
f1(&z.non_settable_reftype.property)
z.non_settable_reftype.property += 1.0
z.non_settable_reftype.property += 1

// regressions with non-settable subscripts in value contexts
_ = z[0] == 0
var d : Double
d = z[0]

// regressions with subscripts that return generic types
var xs:[X]
_ = xs[0].property

struct A<T> {
    subscript(i: Int) -> T { get {} }
}

struct B {
    subscript(i: Int) -> Int { get {} }
}

var a:A<B>

_ = a[0][0]

// Instance members of struct metatypes.
struct FooStruct {
  func instanceFunc0() {}
}

func testFooStruct() {
  FooStruct.instanceFunc0(FooStruct())()
}

// Don't load from explicit lvalues.
func takesInt(_ x: Int) {}
func testInOut(_ arg: inout Int) {
  var x : Int
  takesInt(&x) // expected-error{{'&' used with non-inout argument of type 'Int'}}
}

// Don't infer inout types.
var ir = &i // expected-error {{use of extraneous '&'}}
var ir2 = ((&i)) // expected-error {{use of extraneous '&'}}

// <rdar://problem/17133089>
func takeArrayRef(_ x: inout Array<String>) { }

// rdar://22308291
takeArrayRef(["asdf", "1234"]) // expected-error{{cannot pass immutable value of type '[String]' as inout argument}}

// <rdar://problem/19835413> Reference to value from array changed
func rdar19835413() {
  func f1(_ p: UnsafeMutableRawPointer) {}
  func f2(_ a: [Int], i: Int, pi: UnsafeMutablePointer<Int>) {
    var a = a
    f1(&a)
    f1(&a[i])
    f1(&a[0])
    f1(pi)
    f1(pi)
  }
}

// <rdar://problem/21877598> Crash when accessing stored property without
// setter from constructor
protocol Radish {
  var root: Int { get }
}

public struct Kale : Radish {
  public let root : Int
  public init() {
    let _ = Kale().root
    self.root = 0
  }
}

func testImmutableUnsafePointer(_ p: UnsafePointer<Int>) {
  p.pointee = 1 // expected-error {{cannot assign to property: 'pointee' is a get-only property}}
  p[0] = 1 // expected-error {{cannot assign through subscript: subscript is get-only}}
}

// <https://bugs.swift.org/browse/SR-7> Inferring closure param type to 
// inout crashes compiler
let g = { x in f0(x) } // expected-error{{passing value of type 'Int' to an inout parameter requires explicit '&'}} {{19-19=&}}

// <rdar://problem/17245353> Crash with optional closure taking inout
func rdar17245353() {
  typealias Fn = (inout Int) -> ()
  func getFn() -> Fn? { return nil }

  let _: (inout UInt, UInt) -> Void = { $0 += $1 }
}

// <rdar://problem/23131768> Bugs related to closures with inout parameters
func rdar23131768() {
  func f(_ g: (inout Int) -> Void) { var a = 1; g(&a); print(a) }
  f { $0 += 1 } // Crashes compiler

  func f2(_ g: (inout Int) -> Void) { var a = 1; g(&a); print(a) }
  f2 { $0 = $0 + 1 } // previously error: Cannot convert value of type '_ -> ()' to expected type '(inout Int) -> Void'

  func f3(_ g: (inout Int) -> Void) { var a = 1; g(&a); print(a) }
  f3 { (v: inout Int) -> Void in v += 1 }
}

// <rdar://problem/23331567> Swift: Compiler crash related to closures with inout parameter.
func r23331567(_ fn: (_ x: inout Int) -> Void) {
  var a = 0
  fn(&a)
}
r23331567 { $0 += 1 }

// <rdar://problem/30685195> Compiler crash with invalid assignment
struct G<T> {
  subscript(x: Int) -> T { get { } nonmutating set { } }
  // expected-note@-1 {{'subscript(_:)' declared here}}
}

func wump<T>(to: T, _ body: (G<T>) -> ()) {}

wump(to: 0, { $0[] = 0 })
// expected-error@-1 {{missing argument for parameter #1 in call}}
