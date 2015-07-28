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

func f6<T : P, U : P where T.SomeType == U.SomeType>(t: T, _ u: U) {}

var i : Int
var d : Double

// Check the various forms of diagnostics the type checker can emit.

// Tuple size mismatch.
f1(  // expected-error {{cannot invoke 'f1' with an argument list of type '((Int) -> Int)'}}
   f4 // expected-note @-1 {{expected an argument list of type '((Int, Float) -> Int)'}}
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
   f0 // expected-note @-1 {{expected an argument list of type '((Int, Float) -> Int)'}}
   )

f3( // expected-error {{cannot invoke 'f3' with an argument list of type '((((Int) -> Int)) -> Int)'}}
   f2 // expected-note @-1 {{expected an argument list of type '(((Int) -> Float) -> Int)'}}
   )

// FIXME: Can't test same-type diagnostic yet.
// f4(i, d)

// FIXME: Can't test constructible requirement yet.

// Missing member.
i.wobble() // expected-error{{value of type 'Int' has no member 'wobble'}}

// <rdar://problem/19658691> QoI: Incorrect diagnostic for calling nonexistent members on literals
1.doesntExist(0)  // expected-error {{value of type 'Int' has no member 'doesntExist'}}
[1, 2, 3].doesntExist(0)  // expected-error {{member 'Element' cannot be used on value of type '[Int]'}}
"awfawf".doesntExist(0)   // expected-error {{value of type 'String' has no member 'doesntExist'}}

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
i **** i // expected-error{{binary operator '****' cannot be applied to two 'Int' operands}}
// expected-note @-1 {{expected an argument list of type '(Int, String)'}}

infix operator ***~ {
  associativity left
  precedence 200
}

func ***~(_: Int, _: String) { }
i ***~ i // expected-error{{binary operator '***~' cannot be applied to two 'Int' operands}}
// expected-note @-1 {{expected an argument list of type '(Int, String)'}}

// <rdar://problem/20142523>
// FIXME: poor diagnostic, to be fixed in 20142462. For now, we just want to
// make sure that it doesn't crash.
func rdar20142523() {
  // expected-note @+1 {{overloads for 'map' exist with these partially matching parameter lists: (S, (S.Generator.Element) -> T), (C, (C.Generator.Element) -> T), (T?, @noescape (T) -> U)}}
  map(0..<10, { x in // expected-error{{cannot invoke 'map' with an argument list of type '(Range<Int>, (_) -> _)'}}
    ()
    return x
  })
}

// <rdar://problem/21080030> Bad diagnostic for invalid method call in boolean expression: (_, IntegerLiteralConvertible)' is not convertible to 'IntegerLiteralConvertible
func rdar21080030() {
  var s = "Hello"
  if s.characters.count() == 0 {} // expected-error{{cannot call value of non-function type 'Distance'}}
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
  assert(a != nil, "ASSERT COMPILATION ERROR") // expected-error {{binary operator '!=' cannot be applied to operands of type 'Int' and 'NilLiteralConvertible'}}
  // expected-note @-1 {{expected an argument list of type '(Int, Int)'}}
}


// <rdar://problem/21362748> [WWDC Lab] QoI: cannot subscript a value of type '[Int]?' with an index of type 'Int'
struct StructWithOptionalArray {
  var array: [Int]?
}

func testStructWithOptionalArray(foo: StructWithOptionalArray) -> Int {
  return foo.array[0]  // expected-error {{cannot subscript a value of type '[Int]?'}}
}


// <rdar://problem/19774755> Incorrect diagnostic for unwrapping non-optional bridged types
var invalidForceUnwrap = Int()! // expected-error {{cannot force unwrap value of non-optional type 'Int'}}


// <rdar://problem/20905802> Swift using incorrect diagnostic sometimes on String().asdf
String().asdf  // expected-error {{value of type 'String' has no member 'asdf'}}


// <rdar://problem/21553065> Spurious diagnostic: '_' can only appear in a pattern or on the left side of an assignment
protocol r21553065Protocol {}
class r21553065Class<T : AnyObject> {}
_ = r21553065Class<r21553065Protocol>()  // expected-error {{type 'r21553065Protocol' does not conform to protocol 'AnyObject'}}

// Type variables not getting erased with nested closures
struct Toe {
  let toenail: Nail // expected-error {{use of undeclared type 'Nail'}}

  func clip() {
    toenail.inspect { x in // expected-error {{member 'toenail' cannot be used on value of type 'Toe'}}
      toenail.inspect { y in }
    }
  }
}

// <rdar://problem/21447318> dot'ing through a partially applied member produces poor diagnostic
class r21447318 {
  var x = 42
  func doThing() -> r21447318 { return self }
}

func test21447318(a : r21447318, b : () -> r21447318) {
  a.doThing.doThing()  // expected-error {{method 'doThing' was used as a property; add () to call it}} {{12-12=()}}
  
  b.doThing() // expected-error {{function 'b' was used as a property; add () to call it}} {{4-4=()}}
}

// <rdar://problem/20409366> Diagnostics for init calls should print the class name
class r20409366C {
  init(a : Int) {}
  init?(a : r20409366C) {
    let req = r20409366C(a: 42)?  // expected-error {{cannot use optional chaining on non-optional value of type 'r20409366C'}} {{32-33=}}
  }
}


// <rdar://problem/18800223> QoI: wrong compiler error when swift ternary operator branches don't match
func r18800223(i : Int) {
  // 20099385
  _ = i == 0 ? "" : i  // expected-error {{result values in '? :' expression have mismatching types 'String' and 'Int'}}

  // 19648528
  _ = true ? [i] : i // expected-error {{result values in '? :' expression have mismatching types '[Int]' and 'Int'}}

  
  var buttonTextColor: String?
  _ = (buttonTextColor != nil) ? 42 : {$0}; // expected-error {{type of expression is ambiguous without more context}}
}

// <rdar://problem/21883806> Bogus "'_' can only appear in a pattern or on the left side of an assignment" is back
_ = { $0 }  // expected-error {{type of expression is ambiguous without more context}}



_ = 4()   // expected-error {{invalid use of '()' to call a value of non-function type 'Int'}}
_ = 4(1)  // expected-error {{cannot call value of non-function type 'Int'}}


// <rdar://problem/21784170> Incongruous `unexpected trailing closure` error in `init` function which is cast and called without trailing closure.
func rdar21784170() {
  let initial = (1.0 as Double, 2.0 as Double)
  (Array.init as (Double...) -> Array<Double>)(initial as (Double, Double)) // expected-error {{cannot invoke value of type '(Double...) -> Array<Double>' with argument list '(Double, Double)'}}
}

// <rdar://problem/21829141> BOGUS: unexpected trailing closure
func expect<T, U>(_: T)(_: U.Type) {}
func expect<T, U>(_: T, _: Int = 1)(_: U.Type) {}
expect(Optional(3))(Optional<Int>.self)  // expected-error {{cannot invoke 'expect' with an argument list of type '(Optional<Int>.Type)'}}
// expected-note @-1 {{expected an argument list of type '(U.Type)'}}

// <rdar://problem/19804707> Swift Enum Scoping Oddity
func rdar19804707() {
  enum Op {
    case BinaryOperator((Double, Double) -> Double)
  }
  var knownOps : Op
  knownOps = Op.BinaryOperator({$1 - $0})
  knownOps = Op.BinaryOperator(){$1 - $0}
  knownOps = Op.BinaryOperator{$1 - $0}

  knownOps = .BinaryOperator({$1 - $0})
  knownOps = .BinaryOperator(){$1 - $0} // expected-error {{type of expression is ambiguous without more context}}
  knownOps = .BinaryOperator{$1 - $0}   // expected-error {{type of expression is ambiguous without more context}}
}


// <rdar://problem/20789423> Unclear diagnostic for multi-statement closure with no return type
func r20789423() {
  class C {
    func f(value: Int) { }
  }

  let p: C
  print(p.f(p)())  // expected-error {{cannot invoke 'f' with an argument list of type '(C)'}}
  // expected-note @-1 {{expected an argument list of type '(Int)'}}
  
  let _f = { (v: Int) in  // expected-error {{unable to infer closure type in the current context}}
    // expected-note @-1 {{multi-statement closures require an explicit return type}}
    print("a")
    return "hi"
  }

}



func f7(a: Int)(b : Int) -> Int {
  return a+b
}

f7(1)(b: 1)
f7(1.0)(2)       // expected-error {{cannot invoke 'f7' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(Int)'}}

f7(1)(1.0)       // expected-error {{cannot invoke 'f7' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(b: Int)'}}

let f8 = f7(2)
f8(b: 1)
f8(10)          // expected-error {{missing argument label 'b:' in call}}
f8(1.0)         // expected-error {{cannot invoke 'f8' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(b: Int)'}}

class CurriedClass {
  func method1() {}
  func method2(a: Int)(b : Int) {}
  func method3(a: Int, b : Int) {}
}

let c = CurriedClass()
_ = c.method1
c.method1(1)         // expected-error {{cannot invoke 'method1' with an argument list of type '(Int)'}}
// expected-note @-1 {{expected an argument list of type '()'}}
_ = c.method2(1)
_ = c.method2(1.0)   // expected-error {{cannot invoke 'method2' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(Int)'}}
c.method2(1)(b: 2)
c.method2(1)(c: 2)   // expected-error {{incorrect argument label in call (have 'c:', expected 'b:')}}
c.method2(1)(c: 2.0) // expected-error {{cannot invoke 'method2' with an argument list of type '(c: Double)'}}
// expected-note @-1 {{expected an argument list of type '(b: Int)'}}
c.method2(1)(b: 2.0) // expected-error {{cannot invoke 'method2' with an argument list of type '(b: Double)'}}
// expected-note @-1 {{expected an argument list of type '(b: Int)'}}
c.method2(1.0)(b: 2) // expected-error {{cannot invoke 'method2' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(Int)'}}
c.method2(1.0)(b: 2.0) // expected-error {{cannot invoke 'method2' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(Int)'}}

CurriedClass.method1(c)()
_ = CurriedClass.method1(c)
CurriedClass.method1(c)(1)         // expected-error {{cannot invoke 'method1' with an argument list of type '(Int)'}}
// expected-note @-1 {{expected an argument list of type '()'}}
CurriedClass.method1(2.0)(1)       // expected-error {{cannot invoke 'method1' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(CurriedClass)'}}

CurriedClass.method2(c)(32)(b: 1)
_ = CurriedClass.method2(c)
_ = CurriedClass.method2(c)(32)
_ = CurriedClass.method2(1,2)      // expected-error {{extra argument in call}}
CurriedClass.method2(c)(1.0)(b: 1) // expected-error {{cannot invoke 'method2' with an argument list of type '(Double)'}}
// expected-note @-1 {{expected an argument list of type '(Int)'}}
CurriedClass.method2(c)(1)(b: 1.0) // expected-error {{cannot invoke 'method2' with an argument list of type '(b: Double)'}}
// expected-note @-1 {{expected an argument list of type '(b: Int)'}}
CurriedClass.method2(c)(2)(c: 1.0) // expected-error {{cannot invoke 'method2' with an argument list of type '(c: Double)'}}
// expected-note @-1 {{expected an argument list of type '(b: Int)'}}


CurriedClass.method3(c)(32, b: 1)
_ = CurriedClass.method3(c)
_ = CurriedClass.method3(c)(1, 2)        // expected-error {{missing argument label 'b:' in call}}
_ = CurriedClass.method3(c)(1, b: 2)(32) // expected-error {{cannot call value of non-function type '()'}}
_ = CurriedClass.method3(1, 2)           // expected-error {{extra argument in call}}
CurriedClass.method3(c)(1.0, b: 1)       // expected-error {{cannot invoke 'method3' with an argument list of type '(Double, b: Int)'}}
// expected-note @-1 {{expected an argument list of type '(Int, b: Int)'}}
CurriedClass.method3(c)(1)               // expected-error {{cannot invoke 'method3' with an argument list of type '(Int)'}}
// expected-note @-1 {{expected an argument list of type '(Int, b: Int)'}}

CurriedClass.method3(c)(c: 1.0)          // expected-error {{missing argument for parameter 'b' in call}}


extension CurriedClass {
  func f() {
    method3(1, b: 2)
    method3()            // expected-error {{missing argument for parameter #1 in call}}

    method3(42)          // expected-error {{cannot invoke 'method3' with an argument list of type '(Int)'}}
    // expected-note @-1 {{expected an argument list of type '(Int, b: Int)'}}

    method3(self)        // expected-error {{missing argument for parameter 'b' in call}}
  }
}


// <rdar://problem/19870975> Incorrect diagnostic for failed member lookups within closures passed as arguments ("(_) -> _")
func ident<T>(t: T) -> T {}
var c = ident({1.DOESNT_EXIST}) // error: expected-error {{value of type 'Int' has no member 'DOESNT_EXIST'}}

// <rdar://problem/20712541> QoI: Int/UInt mismatch produces useless error inside a block
var afterMessageCount : Int? = nil

func uintFunc() -> UInt {}
func takeVoidVoidFn(a : () -> ()) {}
takeVoidVoidFn { () -> Void in
  afterMessageCount = uintFunc()  // expected-error {{cannot assign a value of type 'UInt' to a value of type 'Int?'}}
}

// <rdar://problem/19997471> Swift: Incorrect compile error when calling a function inside a closure
func f19997471(x: String) {}
func f19997471(x: Int) {}

func someGeneric19997471<T>(x: T) {
  takeVoidVoidFn {
    f19997471(x) // expected-error {{cannot invoke 'f19997471' with an argument list of type '(T)'}}
     // expected-note @-1 {{overloads for 'f19997471' exist with these partially matching parameter lists: (String), (Int)}}
  }
}

// <rdar://problem/20371273> Type errors inside anonymous functions don't provide enough information
func f20371273() {
  let x: [Int] = [1, 2, 3, 4]
  let y: UInt = 4
  x.filter { $0 == y }  // expected-error {{cannot invoke 'filter' with an argument list of type '((UInt) -> Bool)'}}
   // expected-note @-1 {{expected an argument list of type '(@noescape (Self.Generator.Element) -> Bool)'}}
}




// <rdar://problem/20921068> Swift fails to compile: [0].map() { _ in let r = (1,2).0; return r }
// FIXME: Should complain about not having a return type annotation in the closure.
[0].map { _ in let r =  (1,2).0;  return r }  // expected-error {{cannot invoke 'map' with an argument list of type '((_) -> _)'}}

// <rdar://problem/21078316> Less than useful error message when using map on optional dictionary type
func rdar21078316() {
  var foo : [String : String]?
  var bar : [(String, String)]?
  bar = foo.map { ($0, $1) }  // expected-error {{type of expression is ambiguous without more context}}
}


// <rdar://problem/20978044> QoI: Poor diagnostic when using an incorrect tuple element in a closure
var numbers = [1, 2, 3]
zip(numbers, numbers).filter { $0.1 > 1 && $0.2 > 1 }  // expected-error {{type of expression is ambiguous without more context}}



// <rdar://problem/20868864> QoI: Cannot invoke 'function' with an argument list of type 'type'
func foo20868864(callback: ([String]) -> ()) { }
func rdar20868864(var s: String) {
  foo20868864 { (strings: [String]) in
    s = strings   // expected-error {{cannot assign a value of type '[String]' to a value of type 'String'}}
  }
}

// <rdar://problem/20491794> Error message does not tell me what the problem is
enum Color {
  case Red
  case Unknown(description: String)
}
let _: (Int, Color) = [1,2].map({ ($0, .Unknown("")) }) // expected-error {{type of expression is ambiguous without more context}}


func testTypeSugar(a : Int) {
  typealias Stride = Int

  let x = Stride(a)
  x+"foo"            // expected-error {{binary operator '+' cannot be applied to operands of type 'Stride' (aka 'Int') and 'String'}}
// expected-note @-1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String), (Int, UnsafeMutablePointer<Memory>), (Int, UnsafePointer<Memory>)}}
}

// <rdar://problem/21974772> SegFault in FailureDiagnosis::visitInOutExpr
func r21974772(y : Int) {
  let x = &(1.0 + y)  // expected-error {{binary operator '+' cannot be applied to operands of type 'Double' and 'Int'}}
   //expected-note @-1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (Double, Double), (UnsafeMutablePointer<Memory>, Int), (UnsafePointer<Memory>, Int)}}
}
