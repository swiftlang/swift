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
f1(
   f4 // expected-error {{cannot convert value of type '(Int) -> Int' to expected argument type '(Int, Float) -> Int'}}
   ) 

// Tuple element unused.
f0(i, i,
   i) // expected-error{{extra argument in call}}


// Position mismatch
f5(f4)  // expected-error {{cannot invoke 'f5' with an argument list of type '((Int) -> Int)'}}
// expected-note @-1 {{expected an argument list of type '(T)'}}

// Tuple element not convertible.
f0(i,
   d  // expected-error {{cannot convert value of type 'Double' to expected argument type 'Float'}}
   )

// Function result not a subtype.
f1(
   f0 // expected-error {{cannot convert value of type '(Int, Float) -> ()' to expected argument type '(Int, Float) -> Int'}}
   )

f3(
   f2 // expected-error {{cannot convert value of type '(((Int) -> Int)) -> Int' to expected argument type '((Int) -> Float) -> Int'}}
   )

f4(i, d) // expected-error {{extra argument in call}}

// Missing member.
i.wobble() // expected-error{{value of type 'Int' has no member 'wobble'}}

// <rdar://problem/19658691> QoI: Incorrect diagnostic for calling nonexistent members on literals
1.doesntExist(0)  // expected-error {{value of type 'Int' has no member 'doesntExist'}}
[1, 2, 3].doesntExist(0)  // expected-error {{value of type '[Int]' has no member 'doesntExist'}}
"awfawf".doesntExist(0)   // expected-error {{value of type 'String' has no member 'doesntExist'}}

// Does not conform to protocol.
f5(i)  // expected-error {{cannot invoke 'f5' with an argument list of type '(Int)'}}
// expected-note @-1 {{expected an argument list of type '(T)'}}

// Make sure we don't leave open existentials when diagnosing.
// <rdar://problem/20598568>
func pancakes(p: P2) {
  f4(p.wonka) // expected-error{{cannot convert value of type '() -> ()' to expected argument type 'Int'}}
  f4(p.wonka()) // expected-error{{cannot convert value of type '()' to expected argument type 'Int'}}
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
i **** i // expected-error{{cannot convert value of type 'Int' to expected argument type 'String'}}

infix operator ***~ {
  associativity left
  precedence 200
}

func ***~(_: Int, _: String) { }
i ***~ i // expected-error{{cannot convert value of type 'Int' to expected argument type 'String'}}

// <rdar://problem/20142523>
// FIXME: poor diagnostic, to be fixed in 20142462. For now, we just want to
// make sure that it doesn't crash.
func rdar20142523() {
  map(0..<10, { x in // expected-error{{cannot invoke 'map' with an argument list of type '(Range<Int>, (_) -> _)'}}
    // expected-note @-1 {{overloads for 'map' exist with these partially matching parameter lists: (C, (C.Generator.Element) -> T), (T?, @noescape (T) -> U)}}
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

r21248136()            // expected-error {{generic parameter 'T' could not be inferred}}
let _ = r21248136()    // expected-error {{generic parameter 'T' could not be inferred}}


// <rdar://problem/16375647> QoI: Uncallable funcs should be compile time errors
func perform<T>() {}  // expected-error {{generic parameter 'T' is not used in function signature}}

// <rdar://problem/17080659> Error Message QOI - wrong return type in an overload
func recArea(h: Int, w : Int) {
  return h * w  // expected-error {{no '*' candidates produce the expected contextual result type '()'}}
  // expected-note @-1 {{overloads for '*' exist with these result types: UInt8, Int8, UInt16, Int16, UInt32, Int32, UInt64, Int64, UInt, Int, Float, Double}}
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
    r20201968C.blah()  // expected-error {{use of instance member 'blah' on type 'r20201968C'; did you mean to use a value of type 'r20201968C' instead?}}
  }
}


// <rdar://problem/21459429> QoI: Poor compilation error calling assert
func r21459429(a : Int) {
  assert(a != nil, "ASSERT COMPILATION ERROR") // expected-error {{value of type 'Int' can never be nil, comparison isn't allowed}}
}


// <rdar://problem/21362748> [WWDC Lab] QoI: cannot subscript a value of type '[Int]?' with an index of type 'Int'
struct StructWithOptionalArray {
  var array: [Int]?
}

func testStructWithOptionalArray(foo: StructWithOptionalArray) -> Int {
  return foo.array[0]  // expected-error {{value of optional type '[Int]?' not unwrapped; did you mean to use '!' or '?'?}} {{19-19=!}}
}


// <rdar://problem/19774755> Incorrect diagnostic for unwrapping non-optional bridged types
var invalidForceUnwrap = Int()! // expected-error {{cannot force unwrap value of non-optional type 'Int'}} {{31-32=}}


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
    toenail.inspect { x in
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
  _ = (buttonTextColor != nil) ? 42 : {$0}; // expected-error {{unable to infer closure return type in current context}}
}

// <rdar://problem/21883806> Bogus "'_' can only appear in a pattern or on the left side of an assignment" is back
_ = { $0 }  // expected-error {{unable to infer closure return type in current context}}



_ = 4()   // expected-error {{invalid use of '()' to call a value of non-function type 'Int'}} {{6-8=}}
_ = 4(1)  // expected-error {{cannot call value of non-function type 'Int'}}


// <rdar://problem/21784170> Incongruous `unexpected trailing closure` error in `init` function which is cast and called without trailing closure.
func rdar21784170() {
  let initial = (1.0 as Double, 2.0 as Double)
  (Array.init as (Double...) -> Array<Double>)(initial as (Double, Double)) // expected-error {{cannot convert value of type '(Double, Double)' to expected argument type 'Double'}}
}

// <rdar://problem/21829141> BOGUS: unexpected trailing closure
func expect<T, U>(_: T)(_: U.Type) {}  //  expected-warning{{curried function declaration syntax will be removed in a future version of Swift}}
func expect<T, U>(_: T, _: Int = 1)(_: U.Type) {} //  expected-warning{{curried function declaration syntax will be removed in a future version of Swift}}
expect(Optional(3))(Optional<Int>.self)

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

  // FIXME: rdar://19804707 - These two statements should be accepted by the type checker.
  knownOps = .BinaryOperator(){$1 - $0} // expected-error {{type of expression is ambiguous without more context}}
  knownOps = .BinaryOperator{$1 - $0}   // expected-error {{type of expression is ambiguous without more context}}
}


// <rdar://problem/20789423> Unclear diagnostic for multi-statement closure with no return type
func r20789423() {
  class C {
    func f(value: Int) { }
  }

  let p: C
  print(p.f(p)())  // expected-error {{cannot convert value of type 'C' to expected argument type 'Int'}}

  let _f = { (v: Int) in  // expected-error {{unable to infer closure return type in current context}}
    print("a")
    return "hi"
  }

}



func f7(a: Int)(b : Int) -> Int { // expected-warning{{curried function declaration syntax will be removed in a future version of Swift}}
  return a+b
}

f7(1)(b: 1)
f7(1.0)(2)       // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}

f7(1)(1.0)       // expected-error {{missing argument label 'b:' in call}}
f7(1)(b: 1.0)       // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}

let f8 = f7(2)
f8(b: 1)
f8(10)          // expected-error {{missing argument label 'b:' in call}} {{4-4=b: }}
f8(1.0)         // expected-error {{missing argument label 'b:' in call}}
f8(b: 1.0)         // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}


class CurriedClass {
  func method1() {}
  func method2(a: Int)(b : Int) {} // expected-warning{{curried function declaration syntax will be removed in a future version of Swift}}
  func method3(a: Int, b : Int) {}
}

let c = CurriedClass()
_ = c.method1
c.method1(1)         // expected-error {{argument passed to call that takes no arguments}}
_ = c.method2(1)
_ = c.method2(1.0)   // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
c.method2(1)(b: 2)
c.method2(1)(c: 2)   // expected-error {{incorrect argument label in call (have 'c:', expected 'b:')}} {{14-15=b}}
c.method2(1)(c: 2.0) // expected-error {{incorrect argument label in call (have 'c:', expected 'b:')}}
c.method2(1)(b: 2.0) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
c.method2(1.0)(b: 2) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
c.method2(1.0)(b: 2.0) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}

CurriedClass.method1(c)()
_ = CurriedClass.method1(c)
CurriedClass.method1(c)(1)         // expected-error {{argument passed to call that takes no arguments}}
CurriedClass.method1(2.0)(1)       // expected-error {{use of instance member 'method1' on type 'CurriedClass'; did you mean to use a value of type 'CurriedClass' instead?}}

CurriedClass.method2(c)(32)(b: 1)
_ = CurriedClass.method2(c)
_ = CurriedClass.method2(c)(32)
_ = CurriedClass.method2(1,2)      // expected-error {{use of instance member 'method2' on type 'CurriedClass'; did you mean to use a value of type 'CurriedClass' instead?}}
CurriedClass.method2(c)(1.0)(b: 1) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
CurriedClass.method2(c)(1)(b: 1.0) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
CurriedClass.method2(c)(2)(c: 1.0) // expected-error {{incorrect argument label in call (have 'c:', expected 'b:')}}

CurriedClass.method3(c)(32, b: 1)
_ = CurriedClass.method3(c)
_ = CurriedClass.method3(c)(1, 2)        // expected-error {{missing argument label 'b:' in call}} {{32-32=b: }}
_ = CurriedClass.method3(c)(1, b: 2)(32) // expected-error {{cannot call value of non-function type '()'}}
_ = CurriedClass.method3(1, 2)           // expected-error {{use of instance member 'method3' on type 'CurriedClass'; did you mean to use a value of type 'CurriedClass' instead?}}
CurriedClass.method3(c)(1.0, b: 1)       // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
CurriedClass.method3(c)(1)               // expected-error {{missing argument for parameter 'b' in call}}

CurriedClass.method3(c)(c: 1.0)          // expected-error {{missing argument for parameter 'b' in call}}


extension CurriedClass {
  func f() {
    method3(1, b: 2)
    method3()            // expected-error {{missing argument for parameter #1 in call}}
    method3(42)          // expected-error {{missing argument for parameter 'b' in call}}
    method3(self)        // expected-error {{missing argument for parameter 'b' in call}}
  }
}

extension CurriedClass {
  func m1(a : Int, b : Int) {}
  
  func m2(a : Int) {}
}

// <rdar://problem/23718816> QoI: "Extra argument" error when accidentally currying a method
CurriedClass.m1(2, b: 42)   // expected-error {{use of instance member 'm1' on type 'CurriedClass'; did you mean to use a value of type 'CurriedClass' instead?}}


// <rdar://problem/22108559> QoI: Confusing error message when calling an instance method as a class method
CurriedClass.m2(12)  // expected-error {{use of instance member 'm2' on type 'CurriedClass'; did you mean to use a value of type 'CurriedClass' instead?}}





// <rdar://problem/19870975> Incorrect diagnostic for failed member lookups within closures passed as arguments ("(_) -> _")
func ident<T>(t: T) -> T {}
var c = ident({1.DOESNT_EXIST}) // error: expected-error {{value of type 'Int' has no member 'DOESNT_EXIST'}}

// <rdar://problem/20712541> QoI: Int/UInt mismatch produces useless error inside a block
var afterMessageCount : Int? = nil

func uintFunc() -> UInt {}
func takeVoidVoidFn(a : () -> ()) {}
takeVoidVoidFn { () -> Void in
  afterMessageCount = uintFunc()  // expected-error {{cannot assign value of type 'UInt' to type 'Int?'}}
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
  x.filter { $0 == y }  // expected-error {{binary operator '==' cannot be applied to operands of type 'Int' and 'UInt'}}
  // expected-note @-1 {{overloads for '==' exist with these partially matching parameter lists: (UInt, UInt), (Int, Int)}}
}




// <rdar://problem/20921068> Swift fails to compile: [0].map() { _ in let r = (1,2).0; return r }
// FIXME: Should complain about not having a return type annotation in the closure.
[0].map { _ in let r =  (1,2).0;  return r }
// expected-error @-1 {{cannot invoke 'map' with an argument list of type '(@noescape (Int) throws -> _)'}}
// expected-note @-2 {{expected an argument list of type '(@noescape Int throws -> T)'}}

// <rdar://problem/21078316> Less than useful error message when using map on optional dictionary type
func rdar21078316() {
  var foo : [String : String]?
  var bar : [(String, String)]?
  bar = foo.map { ($0, $1) }  // expected-error {{contextual closure type '([String : String]) -> [(String, String)]' expects 1 argument, but 2 were used in closure body}}
}


// <rdar://problem/20978044> QoI: Poor diagnostic when using an incorrect tuple element in a closure
var numbers = [1, 2, 3]
zip(numbers, numbers).filter { $0.2 > 1 }  // expected-error {{value of tuple type '(Int, Int)' has no member '2'}}



// <rdar://problem/20868864> QoI: Cannot invoke 'function' with an argument list of type 'type'
func foo20868864(callback: ([String]) -> ()) { }

func rdar20868864(s: String) {
  var s = s
  foo20868864 { (strings: [String]) in
    s = strings   // expected-error {{cannot assign value of type '[String]' to type 'String'}}
  }
}

// <rdar://problem/20491794> Error message does not tell me what the problem is
enum Color {
  case Red
  case Unknown(description: String)

  static func rainbow() -> Color {}
  
  static func overload(a a : Int) -> Color {}
  static func overload(b b : Int) -> Color {}
  
  static func frob(a : Int, inout b : Int) -> Color {}
}
let _: (Int, Color) = [1,2].map({ ($0, .Unknown("")) }) // expected-error {{'map' produces '[T]', not the expected contextual result type '(Int, Color)'}}
let _: [(Int, Color)] = [1,2].map({ ($0, .Unknown("")) })// expected-error {{missing argument label 'description:' in call}} {{51-51=description: }}
let _: [Color] = [1,2].map { _ in .Unknown("") }// expected-error {{missing argument label 'description:' in call}} {{44-44=description: }}

let _: Int -> (Int, Color) = { ($0, .Unknown("")) } // expected-error {{missing argument label 'description:' in call}} {{46-46=description: }}
let _: Color = .Unknown("") // expected-error {{missing argument label 'description:' in call}} {{25-25=description: }}
let _: Color = .Unknown // expected-error {{contextual member 'Unknown' expects argument of type '(description: String)'}}
let _: Color = .Unknown(42) // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
let _ : Color = .rainbow(42)  // expected-error {{argument passed to call that takes no arguments}}

let _ : (Int, Float) = (42.0, 12)  // expected-error {{cannot convert value of type 'Double' to specified type 'Int'}}

let _ : Color = .rainbow  // expected-error {{contextual member 'rainbow' expects argument of type '()'}}

let _: Color = .overload(a : 1.0)  // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
let _: Color = .overload(1.0)  // expected-error {{ambiguous reference to member 'overload'}}
// expected-note @-1 {{overloads for 'overload' exist with these partially matching parameter lists: (a: Int), (b: Int)}}
let _: Color = .overload(1)  // expected-error {{ambiguous reference to member 'overload'}}
// expected-note @-1 {{overloads for 'overload' exist with these partially matching parameter lists: (a: Int), (b: Int)}}
let _: Color = .frob(1.0, &i) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
let _: Color = .frob(1, i)  // expected-error {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
let _: Color = .frob(1, &d) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
var someColor : Color = .red // expected-error {{type 'Color' has no member 'red'}}
someColor = .red  // expected-error {{type 'Color' has no member 'red'}}

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

// <rdar://problem/22020088> QoI: missing member diagnostic on optional gives worse error message than existential/bound generic/etc
protocol r22020088P {}

func r22020088Foo<T>(t: T) {}

func r22020088bar(p: r22020088P?) {
  r22020088Foo(p.fdafs)  // expected-error {{value of type 'r22020088P?' has no member 'fdafs'}}
}

// <rdar://problem/22288575> QoI: poor diagnostic involving closure, bad parameter label, and mismatch return type
func f(arguments: [String]) -> [ArraySlice<String>] {
  return arguments.split(1, allowEmptySlices: true, isSeparator: { $0 == "--" })
}



struct AOpts : OptionSetType {
  let rawValue : Int
}

class B {
  func function(x : Int8, a : AOpts) {}
  func f2(a : AOpts) {}
  static func f1(a : AOpts) {}
}


func test(a : B) {
  B.f1(nil)    // expected-error {{nil is not compatible with expected argument type 'AOpts'}}
  a.function(42, a: nil) //expected-error {{nil is not compatible with expected argument type 'AOpts'}}
  a.function(42, nil) //expected-error {{missing argument label 'a:' in call}}
  a.f2(nil)  // expected-error {{nil is not compatible with expected argument type 'AOpts'}}
}

// <rdar://problem/21684487> QoI: invalid operator use inside a closure reported as a problem with the closure
typealias MyClosure = ([Int]) -> Bool
func r21684487() {
  var closures = Array<MyClosure>()
  let testClosure = {(list: [Int]) -> Bool in return true}
  
  let closureIndex = closures.indexOf{$0 === testClosure} // expected-error {{cannot convert value of type 'MyClosure' (aka 'Array<Int> -> Bool') to expected argument type 'AnyObject?'}}
}

// <rdar://problem/18397777> QoI: special case comparisons with nil
func r18397777(d : r21447318?) {
  let c = r21447318()

  if c != nil { // expected-error {{value of type 'r21447318' can never be nil, comparison isn't allowed}}
  }
  
  if d {  // expected-error {{optional type 'r21447318?' cannot be used as a boolean; test for '!= nil' instead}} {{6-6=(}} {{7-7= != nil)}}
  }
  
  if !d { // expected-error {{optional type 'r21447318?' cannot be used as a boolean; test for '== nil' instead}} {{6-7=}} {{7-7=(}} {{8-8= == nil)}}
  }

  if !Optional(c) { // expected-error {{optional type '_' cannot be used as a boolean; test for '== nil' instead}} {{6-7=}} {{7-7=(}} {{18-18= == nil)}}
  }
}


// <rdar://problem/22255907> QoI: bad diagnostic if spurious & in argument list
func r22255907_1<T>(a : T, b : Int) {}
func r22255907_2<T>(x : Int, a : T, b: Int) {}

func reachabilityForInternetConnection() {
  var variable: Int = 42
  r22255907_1(&variable, b: 2.1) // expected-error {{'&' used with non-inout argument of type 'Int'}} {{15-16=}}
  r22255907_2(1, a: &variable, b: 2.1)// expected-error {{'&' used with non-inout argument of type 'Int'}} {{21-22=}}
}

// <rdar://problem/21601687> QoI: Using "=" instead of "==" in if statement leads to incorrect error message
if i = 6 { } // expected-error {{use of '=' in a boolean context, did you mean '=='?}} {{6-7===}}

_ = (i = 6) ? 42 : 57 // expected-error {{use of '=' in a boolean context, did you mean '=='?}} {{8-9===}}


// <rdar://problem/22263468> QoI: Not producing specific argument conversion diagnostic for tuple init
func r22263468(a : String?) {
  typealias MyTuple = (Int, String)
  _ = MyTuple(42, a) // expected-error {{value of optional type 'String?' not unwrapped; did you mean to use '!' or '?'?}} {{20-20=!}}
}


// rdar://22470302 - Crash with parenthesized call result.
class r22470302Class {
  func f() {}
}

func r22470302(c: r22470302Class) {
  print((c.f)(c))  // expected-error {{argument passed to call that takes no arguments}}
}



// <rdar://problem/21928143> QoI: Pointfree reference to generic initializer in generic context does not compile
extension String {
  @available(*, unavailable, message="calling this is unwise")
  func unavail<T : SequenceType where T.Generator.Element == String> // expected-note {{'unavail' has been explicitly marked unavailable here}}
    (a : T) -> String {}
}
extension Array {
  func g() -> String {
    return "foo".unavail([""])  // expected-error {{'unavail' is unavailable: calling this is unwise}}
  }
  
  func h() -> String {
    return "foo".unavail([0])  // expected-error {{value of type 'String' has no member 'Element'}}
  }
}

// <rdar://problem/22519983> QoI: Weird error when failing to infer archetype
func safeAssign<T: RawRepresentable>(inout lhs: T) -> Bool {}  // expected-note {{in call to function 'safeAssign'}}
let a = safeAssign // expected-error {{generic parameter 'T' could not be inferred}}


// <rdar://problem/21692808> QoI: Incorrect 'add ()' fixit with trailing closure
func foo() -> [Int] {
  return Array <Int> (count: 1) { // expected-error {{cannot invoke initializer for type 'Array<Int>' with an argument list of type '(count: Int, () -> Int)'}}
    // expected-note @-1 {{expected an argument list of type '(count: Int, repeatedValue: Element)'}}
    return 1
  }
}



// <rdar://problem/17557899> - This shouldn't suggest calling with ().
func someOtherFunction() {}
func someFunction() -> () {
  // Producing an error suggesting that this
  return someOtherFunction  // expected-error {{unexpected non-void return value in void function}}
}

// <rdar://problem/23560128> QoI: trying to mutate an optional dictionary result produces bogus diagnostic
func r23560128() {
  var a : (Int,Int)?
  a.0 = 42  // expected-error {{value of optional type '(Int, Int)?' not unwrapped; did you mean to use '!' or '?'?}} {{4-4=!}}
}

// <rdar://problem/21890157> QoI: wrong error message when accessing properties on optional structs without unwrapping
struct ExampleStruct21890157 {
  var property = "property"
}
var example21890157: ExampleStruct21890157?
example21890157.property = "confusing"  // expected-error {{value of optional type 'ExampleStruct21890157?' not unwrapped; did you mean to use '!' or '?'?}} {{16-16=!}}


struct UnaryOp {}

_ = -UnaryOp() // expected-error {{unary operator '-' cannot be applied to an operand of type 'UnaryOp'}}
// expected-note @-1 {{overloads for '-' exist with these partially matching parameter lists: (Float), (Double),}}


// <rdar://problem/23433271> Swift compiler segfault in failure diagnosis
func f23433271(x : UnsafePointer<Int>) {}
func segfault23433271(a : UnsafeMutablePointer<Void>) {
  f23433271(a[0])  // expected-error {{cannot convert value of type 'Void' (aka '()') to expected argument type 'UnsafePointer<Int>'}}
}

// <rdar://problem/22058555> crash in cs diags in withCString
func r22058555() {
  var firstChar: UInt8 = 0
  "abc".withCString { chars in
    firstChar = chars[0]  // expected-error {{cannot assign value of type 'Int8' to type 'UInt8'}}
  }
}

// <rdar://problem/23272739> Poor diagnostic due to contextual constraint
func r23272739(contentType: String) {
  let actualAcceptableContentTypes: Set<String> = []
  return actualAcceptableContentTypes.contains(contentType)  // expected-error {{unexpected non-void return value in void function}}
}

// <rdar://problem/23641896> QoI: Strings in Swift cannot be indexed directly with integer offsets
func r23641896() {
  var g = "Hello World"
  g.replaceRange(0...2, with: "ce")  // expected-error {{String may not be indexed with 'Int', it has variable size elements}}
  // expected-note @-1 {{consider using an existing high level algorithm, str.startIndex.advancedBy(n), or a projection like str.utf8}}

  _ = g[12]  // expected-error {{'subscript' is unavailable: cannot subscript String with an Int, see the documentation comment for discussion}}

}

