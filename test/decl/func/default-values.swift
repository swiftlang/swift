// RUN: %swift -parse %s -verify

var func5 : (fn : (Int,Int) -> ()) -> () 

// Default arguments for functions.
def foo3(a: Int = 2, b: Int = 3) {}
def functionCall() {
  foo3(4)
  foo3()
  foo3(a : 4)
  foo3(b : 4)
  foo3(a : 2, b : 4)
}

def g() {}
def h(x: () -> () = g) { x() }

// Tuple types cannot have default values, but recover well here.
def tupleTypes() {
  typealias ta1 = (a : Int = ()) // expected-error{{default argument not permitted in a tuple type}}{{28-32=}}
  var c1 : (a : Int, b : Int, c : Int = 3, // expected-error{{default argument not permitted in a tuple type}}{{39-42=}}
            d = 4) = (1, 2, 3, 4) // expected-error{{default argument not permitted in a tuple type}}{{15-18=}} expected-error{{use of undeclared type 'd'}}
}

def returnWithDefault() -> (a: Int, b: Int = 42) { // expected-error{{default argument not permitted in a tuple type}}
  return 5 // expected-error{{expression does not type-check}}
}

// Only the first parameter list of a curried function can have a
// default argument.
def curried(i: Int = 1)
     (f : Float = 2) { // expected-error{{default argument is only permitted for a non-curried function parameter}}{{17-20=}}
}

// All of the arguments in a selector-style parameter list can have
// default arguments.
def selectorStyle(i: Int = 1) withFloat(f: Float = 2) { }

// Default arguments of constructors.
struct Ctor {
  init (i : Int = 17, f : Float = 1.5) { }
}

Ctor()
Ctor(12)
Ctor(f:12.5)

// Default arguments for nested constructors/functions.
struct Outer<T> {
  struct Inner {
    struct VeryInner {
      init (i : Int = 17, f : Float = 1.5) { }
      static def f(i: Int = 17, f: Float = 1.5) { }
      def g(i: Int = 17, f: Float = 1.5) { }
    }
  }
}
Outer<Int>.Inner.VeryInner()
Outer<Int>.Inner.VeryInner(12)
Outer<Int>.Inner.VeryInner(f:12.5)
Outer<Int>.Inner.VeryInner.f()
Outer<Int>.Inner.VeryInner.f(12)
Outer<Int>.Inner.VeryInner.f(f:12.5)

var vi : Outer<Int>.Inner.VeryInner
vi.g()
vi.g(12)
vi.g(f:12.5)

// <rdar://problem/14564964> crash on invalid
def foo(x: WonkaWibble = 17) { } // expected-error{{use of undeclared type 'WonkaWibble'}}

