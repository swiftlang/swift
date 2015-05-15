// RUN: %target-parse-verify-swift

// Single extraneous keyword argument (tuple-to-scalar)
func f1(a: Int) { }
f1(a: 5) // expected-error{{extraneous argument label 'a:' in call}}{{4-7=}}

struct X1 {
  init(_ a: Int) { }
  func f1(a: Int) {}
}
X1(a: 5).f1(b: 5) // expected-error{{extraneous argument label 'a:' in call}}{{4-7=}}
// expected-error@-1{{extraneous argument label 'b:' in call}}{{13-16=}}

// <rdar://problem/16801056>
enum Policy {
  case Head(Int)
}

func extra2(x x: Int, y: Int) { }

func testExtra2(policy : Policy) {
  switch (policy)
  {
    case .Head(let count):
      extra2(x: 0, y: count)
  }
}

// Single missing keyword argument (scalar-to-tuple)
func f2(a a: Int) { }
f2(5) // expected-error{{missing argument label 'a:' in call}}{{4-4=a: }}

struct X2 {
  init(a: Int) { }
  func f2(b b: Int) { }
}
X2(5).f2(5) // expected-error{{missing argument label 'a:' in call}}{{4-4=a: }}
// expected-error @-1{{missing argument label 'b:' in call}}{{10-10=b: }}


// -------------------------------------------
// Missing keywords
// -------------------------------------------

func allkeywords1(x x: Int, y: Int) { }

// Missing keywords.
allkeywords1(1, 2) // expected-error{{missing argument labels}}
allkeywords1(x: 1, 2) // expected-error{{missing argument label 'y:' in call}}
allkeywords1(1, y: 2) // expected-error{{missing argument label 'x:' in call}}

class GenericCtor<U> {
  init<T>(t : T) {}
}
GenericCtor<Int>()  // expected-error{{missing argument for parameter 't' in call}}

// -------------------------------------------
// Extraneous keywords
// -------------------------------------------
func nokeywords1(x: Int, _ y: Int) { }

nokeywords1(x: 1, y: 1) // expected-error{{extraneous argument labels 'x:y:' in call}}{{13-16=}}{{19-22=}}

// -------------------------------------------
// Some missing, some extraneous keywords
// -------------------------------------------
func somekeywords1(x: Int, y: Int, z: Int) { }

somekeywords1(x: 1, y: 2, z: 3) // expected-error{{extraneous argument label 'x:' in call}}{{15-18=}}
somekeywords1(1, 2, 3) // expected-error{{missing argument labels 'y:z:' in call}}{{18-18=y: }}{{21-21=z: }}
somekeywords1(x: 1, 2, z: 3) // expected-error{{incorrect argument labels in call (have 'x:_:z:', expected '_:y:z:')}}{{15-18=}}{{21-21=y: }}


// -------------------------------------------
// Out-of-order keywords
// -------------------------------------------
allkeywords1(y: 1, x: 2) // expected-error{{argument 'x' must precede argument 'y'}}

// -------------------------------------------
// Default arguments
// -------------------------------------------

func defargs1(x x: Int = 1, y: Int = 2, z: Int = 3) {}

// Using defaults (in-order)
defargs1() 
defargs1(x: 1)
defargs1(x: 1, y: 2)

// Using defaults (in-order, some missing)
defargs1(y: 2)
defargs1(y: 2, z: 3)
defargs1(z: 3)
defargs1(x: 1, z: 3)

// Using defaults (out-of-order, allowed)
defargs1(z: 3, y: 2, x: 1)
defargs1(x: 1, z: 3, y: 2)
defargs1(y: 2, x: 1)

// Default arguments "boxed in".
func defargs2(first first: Int, x: Int = 1, y: Int = 2, z: Int = 3, last: Int) { }

// Using defaults in the middle (in-order, some missing)
defargs2(first: 1, x: 1, z: 3, last: 4)
defargs2(first: 1, x: 1, last: 4)
defargs2(first: 1, y: 2, z: 3, last: 4)
defargs2(first: 1, last: 4)

// Using defaults in the middle (out-of-order, allowed)
defargs2(first: 1, z: 3, x: 1, last: 4)
defargs2(first: 1, z: 3, y: 2, last: 4)

// Using defaults that have moved past a non-defaulted parameter
defargs2(x: 1, first: 1, last: 4) // expected-error{{argument 'first' must precede argument 'x'}}
defargs2(first: 1, last: 4, x: 1) // expected-error{{argument 'x' must precede argument 'last'}}

// -------------------------------------------
// Variadics
// -------------------------------------------
func variadics1(x x: Int, y: Int, _ z: Int...) { }

// Using variadics (in-order, complete)
variadics1(x: 1, y: 2)
variadics1(x: 1, y: 2, 1)
variadics1(x: 1, y: 2, 1, 2)
variadics1(x: 1, y: 2, 1, 2, 3)

// Using various (out-of-order)
// FIXME: Poor diagnostic.
variadics1(1, 2, 3, 4, 5, x: 6, y: 7) // expected-error{{cannot invoke 'variadics1' with an argument list of type '(Int, Int, Int, Int, Int, x: Int, y: Int)'}} expected-note{{expected an argument list of type '(x: Int, y: Int, [Int])'}}

func variadics2(x x: Int, y: Int = 2, z: Int...) { }

// Using variadics (in-order, complete)
variadics2(x: 1, y: 2, z: 1)
variadics2(x: 1, y: 2, z: 1, 2)
variadics2(x: 1, y: 2, z: 1, 2, 3)

// Using variadics (in-order, some missing)
variadics2(x: 1, z: 1, 2, 3)
variadics2(x: 1)

// Using variadics (out-of-order)
variadics2(z: 1, 2, 3, y: 2) // expected-error{{missing argument for parameter 'x' in call}}
variadics2(z: 1, 2, 3, x: 1) // expected-error{{argument 'x' must precede argument 'z'}}

// -------------------------------------------
// Missing arguments
// -------------------------------------------
// FIXME: Diagnostics could be improved with all missing names, or
// simply # of arguments required.
func missingargs1(x x: Int, y: Int, z: Int) {}

missingargs1(x: 1, y: 2) // expected-error{{missing argument for parameter 'z' in call}}

func missingargs2(x x: Int, y: Int, _ z: Int) {}
missingargs2(x: 1, y: 2) // expected-error{{missing argument for parameter #3 in call}}

// -------------------------------------------
// Extra arguments
// -------------------------------------------
// FIXME: Diagnostics could be improved with all extra arguments and
// note pointing to the declaration being called.
func extraargs1(x x: Int) {}

extraargs1(x: 1, y: 2) // expected-error{{extra argument 'y' in call}}
extraargs1(x: 1, 2, 3) // expected-error{{extra argument in call}}

// -------------------------------------------
// Argument name mismatch
// -------------------------------------------

func mismatch1(thisFoo thisFoo: Int = 0, bar: Int = 0, wibble: Int = 0) { }

mismatch1(foo: 5) // expected-error{{incorrect argument label in call (have 'foo:', expected 'thisFoo:')}}
mismatch1(baz: 1, wobble: 2) // expected-error{{incorrect argument labels in call (have 'baz:wobble:', expected 'bar:wibble:')}}
mismatch1(food: 1, zap: 2) // expected-error{{incorrect argument labels in call (have 'food:zap:', expected 'thisFoo:bar:')}}

// -------------------------------------------
// Subscript keyword arguments
// -------------------------------------------

struct Sub1 {
  subscript (i: Int) -> Int {
    get { return i }
  }
}

var sub1 = Sub1()
var i: Int = 0
i = sub1[i]
i = sub1[i: i] // expected-error{{extraneous argument label 'i:' in subscript}}

struct Sub2 {
  subscript (d d: Double) -> Double {
    get { return d }
  }
}

var sub2 = Sub2()
var d: Double = 0.0
d = sub2[d] // expected-error{{missing argument label 'd:' in subscript}}
d = sub2[d: d]
d = sub2[f: d] // expected-error{{incorrect argument label in subscript (have 'f:', expected 'd:')}}

// -------------------------------------------
// Closures
// -------------------------------------------
func intToInt(i: Int) -> Int { return i }

func testClosures() {
  let c0 = { (x: Int, y: Int) in x + y }
  c0(1, 2)

  let c1 = { x, y in intToInt(x + y) }
  c1(1, 2)

  let c2 = { intToInt($0 + $1) }
  c2(1, 2)
}

func acceptAutoclosure(@autoclosure f f: () -> Int) { }
func produceInt() -> Int { }
acceptAutoclosure(f: produceInt) // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}

// -------------------------------------------
// Trailing closures
// -------------------------------------------
func trailingclosure1(x x: Int, f: () -> Int) {}

trailingclosure1(x: 1) { return 5 }
trailingclosure1(1) { return 5 } // expected-error{{missing argument label 'x:' in call}}{{18-18=x: }}

trailingclosure1(x: 1, { return 5 }) // expected-error{{missing argument label 'f:' in call}}

func trailingclosure2(x x: Int, f: (() -> Int)!...) {}
trailingclosure2(x: 5) { return 5 }

func trailingclosure3(x x: Int, var f: (() -> Int)!) { f = nil}
trailingclosure3(x: 5) { return 5 }

func trailingclosure4(f f: () -> Int) {}
trailingclosure4 { 5 }

func trailingClosure5<T>(file: String = __FILE__, line: UInt = __LINE__, expression: () -> T?) { }
func trailingClosure6<T>(value value: Int, expression: () -> T?) { }

trailingClosure5(file: "hello", line: 17) { return Optional.Some(5) } // expected-error{{extraneous argument label 'file:' in call}}{{18-24=}}
trailingClosure6(5) { return Optional.Some(5) } // expected-error{{missing argument label 'value:' in call}}{{18-18=value: }}

class MismatchOverloaded1 {
  func method1(x: Int!, arg: ((Int) -> Int)!) { }
  func method1(x: Int!, secondArg: ((Int) -> Int)!) { }

  @available(*, unavailable)
  func method2(x: Int!, arg: ((Int) -> Int)!) { }

  func method2(x: Int!, secondArg: ((Int) -> Int)!) { }
}

var mismatchOverloaded1 = MismatchOverloaded1()
mismatchOverloaded1.method1(5, arg: nil)
mismatchOverloaded1.method1(5, secondArg: nil)

// Prefer available to unavailable declaration, if it comes up.
mismatchOverloaded1.method2(5) { $0 }

// -------------------------------------------
// Values of function type
// -------------------------------------------
func testValuesOfFunctionType(f1: (_: Int, arg: Int) -> () ) {
  f1(3, arg: 5)
  f1(x: 3, 5) // expected-error{{incorrect argument labels in call (have 'x:_:', expected '_:arg:')}}
  f1(3, 5) // expected-error{{missing argument label 'arg:' in call}}
}


// -------------------------------------------
// Literals
// -------------------------------------------
func string_literals1(x x: String) { }
string_literals1(x: "hello")

func int_literals1(x x: Int) { }
int_literals1(x: 1)

func float_literals1(x x: Double) { }
float_literals1(x: 5)

// -------------------------------------------
// Tuples as arguments
// -------------------------------------------
func produceTuple1() -> (Int, Bool) { return (1, true) }

func acceptTuple1<T>(x: (T, Bool)) { }

acceptTuple1(produceTuple1())
acceptTuple1((1, false))
// FIXME: QoI is awful here.
acceptTuple1(1, false) // expected-error{{extra argument in call}}

func acceptTuple2<T>(input : T) -> T { return input }
var tuple1 = (1, "hello")
acceptTuple2(tuple1)
acceptTuple2((1, "hello", 3.14159))


