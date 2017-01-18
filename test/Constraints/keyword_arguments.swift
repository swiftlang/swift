// RUN: %target-typecheck-verify-swift

// Single extraneous keyword argument (tuple-to-scalar)
func f1(_ a: Int) { }
f1(a: 5) // expected-error{{extraneous argument label 'a:' in call}}{{4-7=}}

struct X1 {
  init(_ a: Int) { }
  func f1(_ a: Int) {}
}
X1(a: 5).f1(b: 5) // expected-error{{extraneous argument label 'a:' in call}}{{4-7=}}

// <rdar://problem/16801056>
enum Policy {
  case Head(Int)
}

func extra2(x: Int, y: Int) { }

func testExtra2(_ policy : Policy) {
  switch (policy)
  {
    case .Head(let count):
      extra2(x: 0, y: count)
  }
}

// Single missing keyword argument (scalar-to-tuple)
func f2(a: Int) { }
f2(5) // expected-error{{missing argument label 'a:' in call}}{{4-4=a: }}

struct X2 {
  init(a: Int) { }
  func f2(b: Int) { }
}
X2(5).f2(5) // expected-error{{missing argument label 'a:' in call}}{{4-4=a: }}


// -------------------------------------------
// Missing keywords
// -------------------------------------------

func allkeywords1(x: Int, y: Int) { }

// Missing keywords.
allkeywords1(1, 2) // expected-error{{missing argument labels}} {{14-14=x: }} {{17-17=y: }}
allkeywords1(x: 1, 2) // expected-error{{missing argument label 'y:' in call}} {{20-20=y: }}
allkeywords1(1, y: 2) // expected-error{{missing argument label 'x:' in call}} {{14-14=x: }}

// If keyword is reserved, make sure to quote it. rdar://problem/21392294
func reservedLabel(_ x: Int, `repeat`: Bool) {}
reservedLabel(1, true) // expected-error{{missing argument label 'repeat:' in call}}{{18-18=repeat: }}

// Insert missing keyword before initial backtick. rdar://problem/21392294 part 2
func reservedExpr(_ x: Int, y: Int) {}
let `do` = 2
reservedExpr(1, `do`) // expected-error{{missing argument label 'y:' in call}}{{17-17=y: }}
reservedExpr(1, y: `do`)

class GenericCtor<U> {
  init<T>(t : T) {}  // expected-note {{'init(t:)' declared here}}
}
GenericCtor<Int>()  // expected-error{{missing argument for parameter 't' in call}}

// -------------------------------------------
// Extraneous keywords
// -------------------------------------------
func nokeywords1(_ x: Int, _ y: Int) { }

nokeywords1(x: 1, y: 1) // expected-error{{extraneous argument labels 'x:y:' in call}}{{13-16=}}{{19-22=}}

// -------------------------------------------
// Some missing, some extraneous keywords
// -------------------------------------------
func somekeywords1(_ x: Int, y: Int, z: Int) { }

somekeywords1(x: 1, y: 2, z: 3) // expected-error{{extraneous argument label 'x:' in call}}{{15-18=}}
somekeywords1(1, 2, 3) // expected-error{{missing argument labels 'y:z:' in call}}{{18-18=y: }}{{21-21=z: }}
somekeywords1(x: 1, 2, z: 3) // expected-error{{incorrect argument labels in call (have 'x:_:z:', expected '_:y:z:')}}{{15-18=}}{{21-21=y: }}


// -------------------------------------------
// Out-of-order keywords
// -------------------------------------------
allkeywords1(y: 1, x: 2) // expected-error{{argument 'x' must precede argument 'y'}} {{14-18=x: 2}} {{20-24=y: 1}}

// -------------------------------------------
// Default arguments
// -------------------------------------------

func defargs1(x: Int = 1, y: Int = 2, z: Int = 3) {}

// Using defaults (in-order)
defargs1() 
defargs1(x: 1)
defargs1(x: 1, y: 2)

// Using defaults (in-order, some missing)
defargs1(y: 2)
defargs1(y: 2, z: 3)
defargs1(z: 3)
defargs1(x: 1, z: 3)

// Using defaults (out-of-order, error by SE-0060)
defargs1(z: 3, y: 2, x: 1) // expected-error{{argument 'y' must precede argument 'z'}} {{10-14=y: 2}} {{16-20=z: 3}}
defargs1(x: 1, z: 3, y: 2) // expected-error{{argument 'y' must precede argument 'z'}} {{16-20=y: 2}} {{22-26=z: 3}}
defargs1(y: 2, x: 1) // expected-error{{argument 'x' must precede argument 'y'}} {{10-14=x: 1}} {{16-20=y: 2}}

// Default arguments "boxed in".
func defargs2(first: Int, x: Int = 1, y: Int = 2, z: Int = 3, last: Int) { }

// Using defaults in the middle (in-order, some missing)
defargs2(first: 1, x: 1, z: 3, last: 4)
defargs2(first: 1, x: 1, last: 4)
defargs2(first: 1, y: 2, z: 3, last: 4)
defargs2(first: 1, last: 4)

// Using defaults in the middle (out-of-order, error by SE-0060)
defargs2(first: 1, z: 3, x: 1, last: 4) // expected-error{{argument 'x' must precede argument 'z'}} {{20-24=x: 1}} {{26-30=z: 3}}
defargs2(first: 1, z: 3, y: 2, last: 4) // expected-error{{argument 'y' must precede argument 'z'}} {{20-24=y: 2}} {{26-30=z: 3}}

// Using defaults that have moved past a non-defaulted parameter
defargs2(x: 1, first: 1, last: 4) // expected-error{{argument 'first' must precede argument 'x'}} {{10-14=first: 1}} {{16-24=x: 1}}
defargs2(first: 1, last: 4, x: 1) // expected-error{{argument 'x' must precede argument 'last'}} {{20-27=x: 1}} {{29-33=last: 4}}

// -------------------------------------------
// Variadics
// -------------------------------------------
func variadics1(x: Int, y: Int, _ z: Int...) { }

// Using variadics (in-order, complete)
variadics1(x: 1, y: 2)
variadics1(x: 1, y: 2, 1)
variadics1(x: 1, y: 2, 1, 2)
variadics1(x: 1, y: 2, 1, 2, 3)

// Using various (out-of-order)
variadics1(1, 2, 3, 4, 5, x: 6, y: 7) // expected-error{{argument 'x' must precede unnamed argument #1}} {{12-25=x: 6}} {{27-31=1, 2, 3, 4, 5}}

func variadics2(x: Int, y: Int = 2, z: Int...) { } // expected-note {{'variadics2(x:y:z:)' declared here}}

// Using variadics (in-order, complete)
variadics2(x: 1, y: 2, z: 1)
variadics2(x: 1, y: 2, z: 1, 2)
variadics2(x: 1, y: 2, z: 1, 2, 3)

// Using variadics (in-order, some missing)
variadics2(x: 1, z: 1, 2, 3)
variadics2(x: 1)

// Using variadics (out-of-order)
variadics2(z: 1, 2, 3, y: 2) // expected-error{{missing argument for parameter 'x' in call}}
variadics2(z: 1, 2, 3, x: 1) // expected-error{{argument 'x' must precede argument 'z'}} {{12-22=x: 1}} {{24-28=z: 1, 2, 3}}

func variadics3(_ x: Int..., y: Int = 2, z: Int = 3) { }

// Using variadics (in-order, complete)
variadics3(1, 2, 3, y: 0, z: 1)
variadics3(1, y: 0, z: 1)
variadics3(y: 0, z: 1)

// Using variadics (in-order, some missing)
variadics3(1, 2, 3, y: 0)
variadics3(1, z: 1)
variadics3(z: 1)

variadics3(1, 2, 3, z: 1)
variadics3(1, z: 1)
variadics3(z: 1)

variadics3(1, 2, 3)
variadics3(1)
variadics3()

// Using variadics (out-of-order)
variadics3(y: 0, 1, 2, 3) // expected-error{{unnamed argument #2 must precede argument 'y'}} {{12-16=1, 2, 3}} {{18-25=y: 0}}
variadics3(z: 1, 1) // expected-error{{unnamed argument #2 must precede argument 'z'}} {{12-16=1}} {{18-19=z: 1}}

func variadics4(x: Int..., y: Int = 2, z: Int = 3) { }

// Using variadics (in-order, complete)
variadics4(x: 1, 2, 3, y: 0, z: 1)
variadics4(x: 1, y: 0, z: 1)
variadics4(y: 0, z: 1)

// Using variadics (in-order, some missing)
variadics4(x: 1, 2, 3, y: 0)
variadics4(x: 1, z: 1)
variadics4(z: 1)

variadics4(x: 1, 2, 3, z: 1)
variadics4(x: 1, z: 1)
variadics4(z: 1)

variadics4(x: 1, 2, 3)
variadics4(x: 1)
variadics4()

// Using variadics (in-order, some missing)
variadics4(y: 0, x: 1, 2, 3) // expected-error{{extra argument in call}}
variadics4(z: 1, x: 1) // expected-error{{argument 'x' must precede argument 'z'}} {{12-16=x: 1}} {{18-22=z: 1}}

func variadics5(_ x: Int, y: Int, _ z: Int...) { } // expected-note {{'variadics5(_:y:_:)' declared here}}

// Using variadics (in-order, complete)
variadics5(1, y: 2)
variadics5(1, y: 2, 1)
variadics5(1, y: 2, 1, 2)
variadics5(1, y: 2, 1, 2, 3)

// Using various (out-of-order)
variadics5(1, 2, 3, 4, 5, 6, y: 7) // expected-error{{argument 'y' must precede unnamed argument #2}} {{15-28=y: 7}} {{30-34=2, 3, 4, 5, 6}}
variadics5(y: 1, 2, 3, 4, 5, 6, 7) // expected-error{{missing argument for parameter #1 in call}}

func variadics6(x: Int..., y: Int = 2, z: Int) { } // expected-note 4 {{'variadics6(x:y:z:)' declared here}}

// Using variadics (in-order, complete)
variadics6(x: 1, 2, 3, y: 0, z: 1)
variadics6(x: 1, y: 0, z: 1)
variadics6(y: 0, z: 1)

// Using variadics (in-order, some missing)
variadics6(x: 1, 2, 3, y: 0) // expected-error{{missing argument for parameter 'z' in call}}
variadics6(x: 1, z: 1)
variadics6(z: 1)

variadics6(x: 1, 2, 3, z: 1)
variadics6(x: 1, z: 1)
variadics6(z: 1)

variadics6(x: 1, 2, 3) // expected-error{{missing argument for parameter 'z' in call}}
variadics6(x: 1) // expected-error{{missing argument for parameter 'z' in call}}
variadics6() // expected-error{{missing argument for parameter 'z' in call}}

func outOfOrder(_ a : Int, b: Int) {
  outOfOrder(b: 42, 52)  // expected-error {{unnamed argument #2 must precede argument 'b'}} {{14-19=52}} {{21-23=b: 42}}
}

// -------------------------------------------
// Missing arguments
// -------------------------------------------
// FIXME: Diagnostics could be improved with all missing names, or
// simply # of arguments required.
func missingargs1(x: Int, y: Int, z: Int) {} // expected-note {{'missingargs1(x:y:z:)' declared here}}

missingargs1(x: 1, y: 2) // expected-error{{missing argument for parameter 'z' in call}}

func missingargs2(x: Int, y: Int, _ z: Int) {} // expected-note {{'missingargs2(x:y:_:)' declared here}}
missingargs2(x: 1, y: 2) // expected-error{{missing argument for parameter #3 in call}}

// -------------------------------------------
// Extra arguments
// -------------------------------------------
// FIXME: Diagnostics could be improved with all extra arguments and
// note pointing to the declaration being called.
func extraargs1(x: Int) {}

extraargs1(x: 1, y: 2) // expected-error{{extra argument 'y' in call}}
extraargs1(x: 1, 2, 3) // expected-error{{extra argument in call}}

// -------------------------------------------
// Argument name mismatch
// -------------------------------------------

func mismatch1(thisFoo: Int = 0, bar: Int = 0, wibble: Int = 0) { }

mismatch1(foo: 5) // expected-error{{incorrect argument label in call (have 'foo:', expected 'thisFoo:')}} {{11-14=thisFoo}}
mismatch1(baz: 1, wobble: 2) // expected-error{{incorrect argument labels in call (have 'baz:wobble:', expected 'bar:wibble:')}} {{11-14=bar}} {{19-25=wibble}}
mismatch1(food: 1, zap: 2) // expected-error{{incorrect argument labels in call (have 'food:zap:', expected 'thisFoo:bar:')}} {{11-15=thisFoo}} {{20-23=bar}}

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
i = sub1[i: i] // expected-error{{extraneous argument label 'i:' in subscript}} {{10-13=}}

struct Sub2 {
  subscript (d d: Double) -> Double {
    get { return d }
  }
}

var sub2 = Sub2()
var d: Double = 0.0
d = sub2[d] // expected-error{{missing argument label 'd:' in subscript}} {{9-9=d: }}
d = sub2[d: d]
d = sub2[f: d] // expected-error{{incorrect argument label in subscript (have 'f:', expected 'd:')}} {{10-11=d}}

// -------------------------------------------
// Closures
// -------------------------------------------
func intToInt(_ i: Int) -> Int { return i }

func testClosures() {
  let c0 = { (x: Int, y: Int) in x + y }
  _ = c0(1, 2)

  let c1 = { x, y in intToInt(x + y) }
  _ = c1(1, 2)

  let c2 = { intToInt($0 + $1) }
  _ = c2(1, 2)
}

func acceptAutoclosure(f: @autoclosure () -> Int) { }
func produceInt() -> Int { }
acceptAutoclosure(f: produceInt) // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}} {{32-32=()}}

// -------------------------------------------
// Trailing closures
// -------------------------------------------
func trailingclosure1(x: Int, f: () -> Int) {}

trailingclosure1(x: 1) { return 5 }
trailingclosure1(1) { return 5 } // expected-error{{missing argument label 'x:' in call}}{{18-18=x: }}

trailingclosure1(x: 1, { return 5 }) // expected-error{{missing argument label 'f:' in call}} {{24-24=f: }}

func trailingclosure2(x: Int, f: (() -> Int)!...) {}
trailingclosure2(x: 5) { return 5 }

func trailingclosure3(x: Int, f: (() -> Int)!) {
  var f = f
  f = nil
  _ = f
}

trailingclosure3(x: 5) { return 5 }

func trailingclosure4(f: () -> Int) {}
trailingclosure4 { 5 }

func trailingClosure5<T>(_ file: String = #file, line: UInt = #line, expression: () -> T?) { }
func trailingClosure6<T>(value: Int, expression: () -> T?) { }

trailingClosure5(file: "hello", line: 17) { return Optional.Some(5) } // expected-error{{extraneous argument label 'file:' in call}}{{18-24=}}
trailingClosure6(5) { return Optional.Some(5) } // expected-error{{missing argument label 'value:' in call}}{{18-18=value: }}

class MismatchOverloaded1 {
  func method1(_ x: Int!, arg: ((Int) -> Int)!) { }
  func method1(_ x: Int!, secondArg: ((Int) -> Int)!) { }

  @available(*, unavailable)
  func method2(_ x: Int!, arg: ((Int) -> Int)!) { }

  func method2(_ x: Int!, secondArg: ((Int) -> Int)!) { }
}

var mismatchOverloaded1 = MismatchOverloaded1()
mismatchOverloaded1.method1(5, arg: nil)
mismatchOverloaded1.method1(5, secondArg: nil)

// Prefer available to unavailable declaration, if it comes up.
mismatchOverloaded1.method2(5) { $0 }

// -------------------------------------------
// Values of function type
// -------------------------------------------
func testValuesOfFunctionType(_ f1: (_: Int, _ arg: Int) -> () ) {
  f1(3, arg: 5) // expected-error{{extraneous argument label 'arg:' in call}}{{9-14=}}
  f1(x: 3, 5) // expected-error{{extraneous argument label 'x:' in call}} {{6-9=}} 
  f1(3, 5)
}


// -------------------------------------------
// Literals
// -------------------------------------------
func string_literals1(x: String) { }
string_literals1(x: "hello")

func int_literals1(x: Int) { }
int_literals1(x: 1)

func float_literals1(x: Double) { }
float_literals1(x: 5)

// -------------------------------------------
// Tuples as arguments
// -------------------------------------------
func produceTuple1() -> (Int, Bool) { return (1, true) }

func acceptTuple1<T>(_ x: (T, Bool)) { }

acceptTuple1(produceTuple1())
acceptTuple1((1, false))
// FIXME: QoI is awful here.
acceptTuple1(1, false) // expected-error{{extra argument in call}}

func acceptTuple2<T>(_ input : T) -> T { return input }
var tuple1 = (1, "hello")
_ = acceptTuple2(tuple1)
_ = acceptTuple2((1, "hello", 3.14159))


