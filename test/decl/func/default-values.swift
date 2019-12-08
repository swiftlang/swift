// RUN: %target-typecheck-verify-swift

var func5 : (_ fn : (Int,Int) -> ()) -> () 

// Default arguments for functions.
func foo3(a: Int = 2, b: Int = 3) {}
func functionCall() {
  foo3(a: 4)
  foo3()
  foo3(a : 4)
  foo3(b : 4)
  foo3(a : 2, b : 4)
}

func g() {}
func h(_ x: () -> () = g) { x() }

// Tuple types cannot have default values, but recover well here.
func tupleTypes() {
  typealias ta1 = (a : Int = ()) // expected-error{{default argument not permitted in a tuple type}}{{28-32=}}
  // expected-error @-1{{cannot create a single-element tuple with an element label}}{{20-24=}}
  var c1 : (a : Int, b : Int, c : Int = 3, // expected-error{{default argument not permitted in a tuple type}}{{39-42=}}
            d = 4) = (1, 2, 3, 4) // expected-error{{default argument not permitted in a tuple type}}{{15-18=}} expected-error{{use of undeclared type 'd'}}
}

func returnWithDefault() -> (a: Int, b: Int = 42) { // expected-error{{default argument not permitted in a tuple type}} {{45-49=}}
  return 5 // expected-error{{cannot convert return expression of type 'Int' to return type '(a: Int, b: Int)'}}
}

func selectorStyle(_ i: Int = 1, withFloat f: Float = 2) { }

// Default arguments of constructors.
struct Ctor {
  init (i : Int = 17, f : Float = 1.5) { }
}

Ctor() // expected-warning{{unused}}
Ctor(i: 12) // expected-warning{{unused}}
Ctor(f:12.5) // expected-warning{{unused}}

// Default arguments for nested constructors/functions.
struct Outer<T> {
  struct Inner {
    struct VeryInner {
      init (i : Int = 17, f : Float = 1.5) { }
      static func f(i: Int = 17, f: Float = 1.5) { }
      func g(i: Int = 17, f: Float = 1.5) { }
    }
  }
}
_ = Outer<Int>.Inner.VeryInner()
_ = Outer<Int>.Inner.VeryInner(i: 12)
_ = Outer<Int>.Inner.VeryInner(f:12.5)
Outer<Int>.Inner.VeryInner.f()
Outer<Int>.Inner.VeryInner.f(i: 12)
Outer<Int>.Inner.VeryInner.f(f:12.5)

var vi : Outer<Int>.Inner.VeryInner
vi.g()
vi.g(i: 12)
vi.g(f:12.5)

// <rdar://problem/14564964> crash on invalid
func foo(_ x: WonkaWibble = 17) { } // expected-error{{use of undeclared type 'WonkaWibble'}}

// Default arguments for initializers.
class SomeClass2 { 
  init(x: Int = 5) {}
}
class SomeDerivedClass2 : SomeClass2 {
  init() {
    super.init()
  }
}

func shouldNotCrash(_ a : UndefinedType, bar b : Bool = true) { // expected-error {{use of undeclared type 'UndefinedType'}}
}

// <rdar://problem/20749423> Compiler crashed while building simple subclass
// code
class SomeClass3 {
  init(x: Int = 5, y: Int = 5) {}
}
class SomeDerivedClass3 : SomeClass3 {}
_ = SomeDerivedClass3()

// Tuple types with default arguments are not materializable
func identity<T>(_ t: T) -> T { return t }
func defaultArgTuplesNotMaterializable(_ x: Int, y: Int = 0) {}

defaultArgTuplesNotMaterializable(identity(5))

// <rdar://problem/22333090> QoI: Propagate contextual information in a call to operands
defaultArgTuplesNotMaterializable(identity((5, y: 10)))
// expected-error@-1 {{cannot convert value of type '(Int, y: Int)' to expected argument type 'Int'}}


// rdar://problem/21799331
func foo<T>(_ x: T, y: Bool = true) {}

foo(true ? "foo" : "bar")

func foo2<T>(_ x: T, y: Bool = true) {}

extension Array {
  func bar(_ x: (Element) -> Bool) -> Int? { return 0 }
}

foo2([].bar { $0 == "c" }!)

// rdar://problem/21643052
let a = ["1", "2"].map { Int($0) }

// Default arguments for static members used via ".foo"
struct X<T> {
  static func foo(i: Int, j: Int = 0) -> X {
    return X()
  }

  static var bar: X { return X() }
}

let testXa: X<Int> = .foo(i: 0)
let testXb: X<Int> = .bar

// SR-10062

var aLiteral = 1
let bLiteral = 2

func inoutFuncWithDefaultArg1(x: inout Int = 1) {} // expected-error {{cannot provide default value to inout parameter 'x'}}
func inoutFuncWithDefaultArg2(x: inout Int = bLiteral) {} // expected-error {{cannot provide default value to inout parameter 'x'}}
func inoutFuncWithDefaultArg3(x: inout Int = aLiteral) {} // expected-error {{cannot provide default value to inout parameter 'x'}}
func inoutFuncWithDefaultArg4(x: inout Int = &aLiteral) {} // expected-error {{cannot provide default value to inout parameter 'x'}}
// expected-error@-1 {{use of extraneous '&'}}

func inoutFuncWithDefaultArg5(x: inout Int = &bLiteral) {} // expected-error {{cannot provide default value to inout parameter 'x'}}
// expected-error@-1 {{use of extraneous '&'}}

func inoutFuncWithDefaultArg6(x: inout Int = #file) {} // expected-error {{cannot provide default value to inout parameter 'x'}}
// expected-error@-1 {{default argument value of type 'String' cannot be converted to type 'Int'}}

func inoutFuncWithDefaultArg7(_: inout Int = 1) {} // expected-error {{cannot provide default value to inout parameter '_'}}

// SE-0242 - Test that memberwise constructor generates default values

struct Foo {
  var a: Int
  var b: Bool = false
  let c: (Int, Bool) = (1, true)
  let d: Int
  var (e, f) = (0, false)
  var g: Int?
  let h: Bool?

  // The generated memberwise should look like the following:
  // init(a: Int, b: Bool = false, d: Int, e: Int, f: Bool, g: Int? = nil, h: Bool?)
}

// Here b = false and g = nil
let fooThing1 = Foo(a: 0, d: 1, e: 2, f: false, h: nil) // ok
// Here g = nil
let fooThing2 = Foo(a: 0, b: true, d: 1, e: 2, f: false, h: nil) // ok
// Here b = false
let fooThing3 = Foo(a: 0, d: 1, e: 2, f: false, g: 10, h: nil) // ok
// Use all the parameters
let fooThing4 = Foo(a: 0, b: true, d: 1, e: 2, f: false, g: 10, h: nil) // ok

// Ensure that tuple init is not allowed
// Here b = false and g = nil, but we're checking that e and f don't get a default value
let fooThing5 = Foo(a: 0, d: 1, h: nil) // expected-error {{missing arguments for parameters 'e', 'f' in call}}
                                        // expected-note@-25 {{'init(a:b:d:e:f:g:h:)' declared here}}

// Here b = false and g = nil, but we're checking that f doesn't get a default value
let fooThing6 = Foo(a: 0, d: 1, e: 2, h: nil) // expected-error {{missing argument for parameter 'f' in call}}
                                              // expected-note@-29 {{'init(a:b:d:e:f:g:h:)' declared here}}

// SR-11085
func sr_11085(x: Int) {}
func sr_11085(line: String = #line) {} // expected-error {{default argument value of type 'Int' cannot be converted to type 'String'}}
sr_11085()

class SR_11085_C { init(line: String = #line) {} } // expected-error {{default argument value of type 'Int' cannot be converted to type 'String'}}
let _ = SR_11085_C()

// SR-11623
func badGenericMagicLiteral<T : ExpressibleByIntegerLiteral>(_ x: T = #function) -> T { x } // expected-error {{default argument value of type 'String' cannot be converted to type 'T'}}
let _: Int = badGenericMagicLiteral()

func genericMagicLiteral<T : ExpressibleByIntegerLiteral>(_ x: T = #line) -> T { x } // expected-note {{where 'T' = 'String'}}
let _: Int = genericMagicLiteral()
let _: String = genericMagicLiteral() // expected-error {{global function 'genericMagicLiteral' requires that 'String' conform to 'ExpressibleByIntegerLiteral'}}
