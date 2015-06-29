// RUN: %target-parse-verify-swift

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
        _ y: Float) { }

func f1(_: (Int, Float) -> Int) { }

func f2(_: (_: (Int) -> Int)) -> Int {}

func f3(_: (_: (Int) -> Float) -> Int) {}

func f4(x: Int) -> Int { }

func f5<T : P2>(_ : T) { }

func f10<T : P, U : P where T.SomeType == U.SomeType>(t: T, _ u: U) {}

var i : Int
var d : Double

// Check the various forms of diagnostics the type checker can emit.

// Tuple size mismatch.
f1(  // expected-error {{cannot invoke 'f1' with an argument list of type '((Int) -> Int)'}}
   f4 // expected-note {{expected an argument list of type '((Int, Float) -> Int)'}}
   ) 

// Tuple element unused.
f0(i, i, // expected-error{{extra argument in call}}
   i)

// FIXME: Tuple name mismatch.

// FIXME: Position mismatch
// f5(f4)

// Tuple element not convertible.
f0(i, // expected-error {{cannot invoke 'f0' with an argument list of type '(Int, Double)'}} expected-note{{expected an argument list of type '(Int, Float)'}}
   d
   )

// Function result not a subtype.
f1( // expected-error {{cannot invoke 'f1' with an argument list of type '((Int, Float) -> ())'}}
   f0 // expected-note{{expected an argument list of type '((Int, Float) -> Int)'}}
   )

f3( // expected-error {{cannot invoke 'f3' with an argument list of type '((((Int) -> Int)) -> Int)'}}
   f2 // expected-note{{expected an argument list of type '(((Int) -> Float) -> Int)'}}
   )

// FIXME: Can't test same-type diagnostic yet.
// f4(i, d)

// FIXME: Can't test constructible requirement yet.

// Missing member.
i.wobble() // expected-error{{'Int' does not have a member named 'wobble'}}

// Does not conform to protocol.
// FIXME: f5(i)

// Make sure we don't leave open existentials when diagnosing.
// <rdar://problem/20598568>
func pancakes(p: P2) {
  f4(p.wonka) // expected-error{{cannot invoke 'f4' with an argument list of type '(() -> ())'}}
  // expected-note@-1{{expected an argument list of type '(Int)'}}
  f4(p.wonka()) // expected-error{{cannot invoke 'f4' with an argument list of type '()'}}
  // expected-note@-1{{expected an argument list of type '(Int)'}}
}

protocol Shoes {
  static func select(subject: Shoes) -> Self
}

// Here the opaque value has type (metatype_type (archetype_type ... ))
func f(x: Shoes, asType t: Shoes.Type) {
  return t.select(x) // expected-error{{unexpected non-void return value in void function}}
}

infix operator **** {
  associativity left
  precedence 200
}

func ****(_: Int, _: String) { }
i **** i // expected-error{{binary operator '****' cannot be applied to two Int operands}}

infix operator ***~ {
  associativity left
  precedence 200
}

func ***~(_: Int, _: String) { }
i ***~ i // expected-error{{binary operator '***~' cannot be applied to two Int operands}}

// <rdar://problem/20142523>
// FIXME: poor diagnostic, to be fixed in 20142462. For now, we just want to
// make sure that it doesn't crash.
func rdar20142523() {
  map(0..<10, { x in // expected-error{{could not find an overload for '..<' that accepts the supplied arguments}}
    ()
    return x
  })
}

func rdar21080030() {
  var s = "Hello"
  if s.characters.count() == 0 {} // expected-error{{cannot invoke 'count' with no arguments}}
}

// <rdar://problem/21248136> QoI: problem with return type inference mis-diagnosed as invalid arguments
func r21248136<T>() -> T { preconditionFailure() } // expected-note 2 {{in call to function 'r21248136'}}

r21248136()            // expected-error {{argument for generic parameter 'T' could not be inferred}}
let _ = r21248136()    // expected-error {{argument for generic parameter 'T' could not be inferred}}


// <rdar://problem/16375647> QoI: Uncallable funcs should be compile time errors
func perform<T>() {}  // expected-error {{generic parameter 'T' is not used in function signature}}

// <rdar://problem/17080659> Error Message QOI - wrong return type in an overload
func recArea(h: Int, w : Int) {
  return h * w  // expected-error {{unexpected non-void return value in void function}}
}

// <rdar://problem/17224804> QoI: Error In Ternary Condition is Wrong
func r17224804(monthNumber : Int) {
  // expected-error @+2 {{binary operator '+' cannot be applied to operands of type 'String' and 'Int'}}
  // expected-note @+1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String), (UnsafeMutablePointer<Memory>, Int), (UnsafePointer<Memory>, Int)}}
  let monthString = (monthNumber <= 9) ? ("0" + monthNumber) : String(monthNumber)
}

// <rdar://problem/17020197> QoI: Operand of postfix '!' should have optional type; type is 'Int?'
func r17020197(x : Int?, y : Int) {
  if x! {  }  // expected-error {{type 'Int' does not conform to protocol 'BooleanType'}}

  // <rdar://problem/12939553> QoI: diagnostic for using an integer in a condition is utterly terrible
  if y {}    // expected-error {{type 'Int' does not conform to protocol 'BooleanType'}}
}

// <rdar://problem/20714480> QoI: Boolean expr not treated as Bool type when function return type is different
func validateSaveButton(text: String) {
  return (text.characters.count > 0) ? true : false  // expected-error {{unexpected non-void return value in void function}}
}

// <rdar://problem/20201968> QoI: poor diagnostic when calling a class method via a metatype
class r20201968C {
  func blah() {
    r20201968C.blah()  // expected-error {{missing argument for parameter #1 in call}}
  }
}


// <rdar://problem/21459429> QoI: Poor compilation error calling assert
func r21459429(a : Int) {
  assert(a != nil, "ASSERT COMPILATION ERROR") // expected-error {{cannot invoke '!=' with an argument list of type '(Int, nil)'}}
}
