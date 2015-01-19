// RUN: %target-parse-verify-swift

var func5 : (fn : (Int,Int) -> ()) -> () 

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
func h(x: () -> () = g) { x() }

// Tuple types cannot have default values, but recover well here.
func tupleTypes() {
  typealias ta1 = (a : Int = ()) // expected-error{{default argument not permitted in a tuple type}}{{28-32=}}
  // expected-error @-1{{cannot create a single-element tuple with an element label}}{{20-24=}}
  var c1 : (a : Int, b : Int, c : Int = 3, // expected-error{{default argument not permitted in a tuple type}}{{39-42=}}
            d = 4) = (1, 2, 3, 4) // expected-error{{default argument not permitted in a tuple type}}{{15-18=}} expected-error{{use of undeclared type 'd'}}
}

func returnWithDefault() -> (a: Int, b: Int = 42) { // expected-error{{default argument not permitted in a tuple type}}
  return 5 // expected-error{{'Int' is not convertible to '(a: Int, b: Int)'}}
}

// Only the first parameter list of a curried function can have a
// default argument.
func curried(i: Int = 1)
     (f : Float = 2) { // expected-error{{default argument is only permitted for a non-curried function parameter}}{{17-20=}}
}

func selectorStyle(i: Int = 1, withFloat f: Float = 2) { }

// Default arguments of constructors.
struct Ctor {
  init (i : Int = 17, f : Float = 1.5) { }
}

Ctor()
Ctor(i: 12)
Ctor(f:12.5)

// Default arguments for nested constructors/functions.
struct Outer<T> {
  struct Inner { // expected-error{{type 'Inner' nested in generic type}}
    struct VeryInner {// expected-error{{type 'VeryInner' nested in generic type}}
      init (i : Int = 17, f : Float = 1.5) { }
      static func f(i: Int = 17, f: Float = 1.5) { }
      func g(i: Int = 17, f: Float = 1.5) { }
    }
  }
}
Outer<Int>.Inner.VeryInner()
Outer<Int>.Inner.VeryInner(i: 12)
Outer<Int>.Inner.VeryInner(f:12.5)
Outer<Int>.Inner.VeryInner.f()
Outer<Int>.Inner.VeryInner.f(i: 12)
Outer<Int>.Inner.VeryInner.f(f:12.5)

var vi : Outer<Int>.Inner.VeryInner
vi.g()
vi.g(i: 12)
vi.g(f:12.5)

// <rdar://problem/14564964> crash on invalid
func foo(x: WonkaWibble = 17) { } // expected-error{{use of undeclared type 'WonkaWibble'}}

// Default arguments for initializers.
class SomeClass2 { 
  init(x: Int = 5) {}
}
class SomeDerivedClass2 : SomeClass2 {
  init() {
    super.init()
  }
}

func shouldNotCrash(a : UndefinedType, bar b : Bool = true) { // expected-error {{use of undeclared type 'UndefinedType'}}
}

