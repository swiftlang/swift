// RUN: %swift -parse %s -verify

protocol P {
  typealias SomeType
}

protocol P2 { 
  func wonka()
}

extension Int : P {
  typealias SomeType = Int
}

extension Double : P {
  typealias SomeType = Double
}

func f0(x: Int, 
       y: Float) { } // expected-note{{in initialization of parameter 'y'}}

func f1(_: (Int, Float) -> Int) { } // expected-note 2{{in call to function 'f1'}}

func f2(_: (_: (Int) -> Int)) -> Int {}

func f3(_: (_: (Int) -> Float) -> Int) {} // expected-note{{in call to function 'f3'}}

func f4(x: Int) -> Int { }

func f5<T : P2>(_ : T) { }

func f10<T : P, U : P where T.SomeType == U.SomeType>(t: T, u: U) {}

var i : Int
var d : Double

// Check the various forms of diagnostics the type checker can emit.

// Tuple size mismatch.
f1( 
   f4 // expected-error{{'(Int, Float)' is not a subtype of 'Int'}}
   ) 

// Tuple element unused.
f0(i, i, // expected-error{{extra argument in call}}
   i)

// FIXME: Tuple name mismatch.

// FIXME: Position mismatch
// f5(f4)

// Tuple element not convertible.
f0(i,
   d // expected-error{{'Double' is not convertible to 'Float'}}
   )

// Function result not a subtype.
f1(
   f0 // expected-error{{'()' is not a subtype of 'Int'}}
   )

f3( 
   f2 // expected-error{{'Float' is not a subtype of 'Int'}}
   )

// FIXME: Can't test same-type diagnostic yet.
// f4(i, d)

// FIXME: Can't test constructible requirement yet.

// Missing member.
i.wobble() // expected-error{{Int' does not have a member named 'wobble'}}

// Does not conform to protocol.
// FIXME: f5(i)

operator infix **** {
  associativity left
  precedence 200
}

func ****(_: Int, _: String) { } // expected-note{{in initialization of parameter '_'}}
i **** i // expected-error{{'Int' is not convertible to 'String'}}

operator infix ***~ {
  associativity left
  precedence 200
}

func ***~(Int, String) { } // expected-note{{in initialization of parameter '_'}}
i ***~ i // expected-error{{'Int' is not convertible to 'String'}}
