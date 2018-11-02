// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype SomeType
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

func f0(_ x: Int, 
        _ y: Float) { }

func f1(_: @escaping (Int, Float) -> Int) { }

func f2(_: (_: (Int) -> Int)) -> Int {}

func f3(_: @escaping (_: @escaping (Int) -> Float) -> Int) {}

func f4(_ x: Int) -> Int { }

func f5<T : P2>(_ : T) { }

func f6<T : P, U : P>(_ t: T, _ u: U) where T.SomeType == U.SomeType {}

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
f5(f4)  // expected-error {{argument type '(Int) -> Int' does not conform to expected type 'P2'}}

// Tuple element not convertible.
f0(i,
   d  // expected-error {{cannot convert value of type 'Double' to expected argument type 'Float'}}
   )

// Function result not a subtype.
f1(
   f0 // expected-error {{cannot convert value of type '(Int, Float) -> ()' to expected argument type '(Int, Float) -> Int'}}
   )

f3(
   f2 // expected-error {{cannot convert value of type '(@escaping ((Int) -> Int)) -> Int' to expected argument type '(@escaping (Int) -> Float) -> Int'}}
   )

f4(i, d) // expected-error {{extra argument in call}}

// Missing member.
i.wobble() // expected-error{{value of type 'Int' has no member 'wobble'}}

// Generic member does not conform.
extension Int {
  func wibble<T: P2>(_ x: T, _ y: T) -> T { return x }
  func wubble<T>(_ x: (Int) -> T) -> T { return x(self) }
}
i.wibble(3, 4) // expected-error {{argument type 'Int' does not conform to expected type 'P2'}}

// Generic member args correct, but return type doesn't match.
struct A : P2 {
  func wonka() {}
}
let a = A()
for j in i.wibble(a, a) { // expected-error {{type 'A' does not conform to protocol 'Sequence'}}
}

// Generic as part of function/tuple types
func f6<T:P2>(_ g: (Void) -> T) -> (c: Int, i: T) { // expected-warning {{when calling this function in Swift 4 or later, you must pass a '()' tuple; did you mean for the input type to be '()'?}} {{20-26=()}}
  return (c: 0, i: g(()))
}

func f7() -> (c: Int, v: A) {
  let g: (Void) -> A = { _ in return A() } // expected-warning {{when calling this function in Swift 4 or later, you must pass a '()' tuple; did you mean for the input type to be '()'?}} {{10-16=()}}
  return f6(g) // expected-error {{cannot convert return expression of type '(c: Int, i: A)' to return type '(c: Int, v: A)'}}
}

func f8<T:P2>(_ n: T, _ f: @escaping (T) -> T) {}
f8(3, f4) // expected-error {{in argument type '(Int) -> Int', 'Int' does not conform to expected type 'P2'}}
typealias Tup = (Int, Double)
func f9(_ x: Tup) -> Tup { return x }
f8((1,2.0), f9) // expected-error {{in argument type '(Tup) -> Tup' (aka '((Int, Double)) -> (Int, Double)'), 'Tup' (aka '(Int, Double)') does not conform to expected type 'P2'}}

// <rdar://problem/19658691> QoI: Incorrect diagnostic for calling nonexistent members on literals
1.doesntExist(0)  // expected-error {{value of type 'Int' has no member 'doesntExist'}}
[1, 2, 3].doesntExist(0)  // expected-error {{value of type '[Int]' has no member 'doesntExist'}}
"awfawf".doesntExist(0)   // expected-error {{value of type 'String' has no member 'doesntExist'}}

// Does not conform to protocol.
f5(i)  // expected-error {{argument type 'Int' does not conform to expected type 'P2'}}

// Make sure we don't leave open existentials when diagnosing.
// <rdar://problem/20598568>
func pancakes(_ p: P2) {
  f4(p.wonka) // expected-error{{cannot convert value of type '() -> ()' to expected argument type 'Int'}}
  f4(p.wonka()) // expected-error{{cannot convert value of type '()' to expected argument type 'Int'}}
}

protocol Shoes {
  static func select(_ subject: Shoes) -> Self
}

// Here the opaque value has type (metatype_type (archetype_type ... ))
func f(_ x: Shoes, asType t: Shoes.Type) {
  return t.select(x) // expected-error{{unexpected non-void return value in void function}}
}

precedencegroup Starry {
  associativity: left
  higherThan: MultiplicationPrecedence
}

infix operator **** : Starry

func ****(_: Int, _: String) { }
i **** i // expected-error{{cannot convert value of type 'Int' to expected argument type 'String'}}

infix operator ***~ : Starry

func ***~(_: Int, _: String) { }
i ***~ i // expected-error{{cannot convert value of type 'Int' to expected argument type 'String'}}

@available(*, unavailable, message: "call the 'map()' method on the sequence")
public func myMap<C : Collection, T>( // expected-note {{'myMap' has been explicitly marked unavailable here}}
  _ source: C, _ transform: (C.Iterator.Element) -> T
) -> [T] {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, message: "call the 'map()' method on the optional value")
public func myMap<T, U>(_ x: T?, _ f: (T) -> U) -> U? {
  fatalError("unavailable function can't be called")
}

// <rdar://problem/20142523>
func rdar20142523() {
  myMap(0..<10, { x in // expected-error{{'myMap' is unavailable: call the 'map()' method on the sequence}}
    ()
    return x
  })
}

// <rdar://problem/21080030> Bad diagnostic for invalid method call in boolean expression: (_, ExpressibleByIntegerLiteral)' is not convertible to 'ExpressibleByIntegerLiteral
func rdar21080030() {
  var s = "Hello"
  // SR-7599: This should be `cannot_call_non_function_value`
  if s.count() == 0 {} // expected-error{{cannot invoke 'count' with no arguments}}
}

// <rdar://problem/21248136> QoI: problem with return type inference mis-diagnosed as invalid arguments
func r21248136<T>() -> T { preconditionFailure() } // expected-note 2 {{in call to function 'r21248136()'}}

r21248136()            // expected-error {{generic parameter 'T' could not be inferred}}
let _ = r21248136()    // expected-error {{generic parameter 'T' could not be inferred}}


// <rdar://problem/16375647> QoI: Uncallable funcs should be compile time errors
func perform<T>() {}  // expected-error {{generic parameter 'T' is not used in function signature}}

// <rdar://problem/17080659> Error Message QOI - wrong return type in an overload
func recArea(_ h: Int, w : Int) {
  return h * w  // expected-error {{unexpected non-void return value in void function}}
}

// <rdar://problem/17224804> QoI: Error In Ternary Condition is Wrong
func r17224804(_ monthNumber : Int) {
  // expected-error @+2 {{binary operator '+' cannot be applied to operands of type 'String' and 'Int'}}
  // expected-note @+1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String)}}
  let monthString = (monthNumber <= 9) ? ("0" + monthNumber) : String(monthNumber)
}

// <rdar://problem/17020197> QoI: Operand of postfix '!' should have optional type; type is 'Int?'
func r17020197(_ x : Int?, y : Int) {
  if x! {  }  // expected-error {{'Int' is not convertible to 'Bool'}}

  // <rdar://problem/12939553> QoI: diagnostic for using an integer in a condition is utterly terrible
  if y {}    // expected-error {{'Int' is not convertible to 'Bool'}}
}

// <rdar://problem/20714480> QoI: Boolean expr not treated as Bool type when function return type is different
func validateSaveButton(_ text: String) {
  return (text.count > 0) ? true : false  // expected-error {{unexpected non-void return value in void function}}
}

// <rdar://problem/20201968> QoI: poor diagnostic when calling a class method via a metatype
class r20201968C {
  func blah() {
    r20201968C.blah()  // expected-error {{instance member 'blah' cannot be used on type 'r20201968C'; did you mean to use a value of this type instead?}}
  }
}


// <rdar://problem/21459429> QoI: Poor compilation error calling assert
func r21459429(_ a : Int) {
  assert(a != nil, "ASSERT COMPILATION ERROR")
  // expected-warning @-1 {{comparing non-optional value of type 'Int' to 'nil' always returns true}}
}


// <rdar://problem/21362748> [WWDC Lab] QoI: cannot subscript a value of type '[Int]?' with an index of type 'Int'
struct StructWithOptionalArray {
  var array: [Int]?
}

func testStructWithOptionalArray(_ foo: StructWithOptionalArray) -> Int {
  return foo.array[0]  // expected-error {{value of optional type '[Int]?' must be unwrapped to refer to member 'subscript' of wrapped base type '[Int]'}}
  // expected-note@-1{{chain the optional using '?' to access member 'subscript' only for non-'nil' base values}}{{19-19=?}}
  // expected-note@-2{{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}{{19-19=!}}
}


// <rdar://problem/19774755> Incorrect diagnostic for unwrapping non-optional bridged types
var invalidForceUnwrap = Int()! // expected-error {{cannot force unwrap value of non-optional type 'Int'}} {{31-32=}}


// <rdar://problem/20905802> Swift using incorrect diagnostic sometimes on String().asdf
String().asdf  // expected-error {{value of type 'String' has no member 'asdf'}}


// <rdar://problem/21553065> Spurious diagnostic: '_' can only appear in a pattern or on the left side of an assignment
protocol r21553065Protocol {}
class r21553065Class<T : AnyObject> {} // expected-note{{requirement specified as 'T' : 'AnyObject'}}
_ = r21553065Class<r21553065Protocol>()  // expected-error {{'r21553065Class' requires that 'r21553065Protocol' be a class type}}

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

func test21447318(_ a : r21447318, b : () -> r21447318) {
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
func r18800223(_ i : Int) {
  // 20099385
  _ = i == 0 ? "" : i  // expected-error {{result values in '? :' expression have mismatching types 'String' and 'Int'}}

  // 19648528
  _ = true ? [i] : i // expected-error {{result values in '? :' expression have mismatching types '[Int]' and 'Int'}}

  
  var buttonTextColor: String?
  _ = (buttonTextColor != nil) ? 42 : {$0}; // expected-error {{type of expression is ambiguous without more context}}
}

// <rdar://problem/21883806> Bogus "'_' can only appear in a pattern or on the left side of an assignment" is back
_ = { $0 }  // expected-error {{unable to infer closure type in the current context}}



_ = 4()   // expected-error {{cannot call value of non-function type 'Int'}}{{6-8=}}
_ = 4(1)  // expected-error {{cannot call value of non-function type 'Int'}}


// <rdar://problem/21784170> Incongruous `unexpected trailing closure` error in `init` function which is cast and called without trailing closure.
func rdar21784170() {
  let initial = (1.0 as Double, 2.0 as Double)
  (Array.init as (Double...) -> Array<Double>)(initial as (Double, Double)) // expected-error {{cannot convert value of type '(Double, Double)' to expected argument type 'Double'}}
}

// <rdar://problem/21829141> BOGUS: unexpected trailing closure
func expect<T, U>(_: T) -> (U.Type) -> Int { return { a in 0 } }
func expect<T, U>(_: T, _: Int = 1) -> (U.Type) -> String { return { a in "String" } }
let expectType1 = expect(Optional(3))(Optional<Int>.self)
let expectType1Check: Int = expectType1

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

  // rdar://19804707 - trailing closures for contextual member references.
  knownOps = .BinaryOperator(){$1 - $0}
  knownOps = .BinaryOperator{$1 - $0}

  _ = knownOps
}


func f7(_ a: Int) -> (_ b: Int) -> Int {
  return { b in a+b }
}

_ = f7(1)(1)
f7(1.0)(2)       // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}

f7(1)(1.0)       // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
f7(1)(b: 1.0)    // expected-error{{extraneous argument label 'b:' in call}}   

let f8 = f7(2)
_ = f8(1)
f8(10)          // expected-warning {{result of call to function returning 'Int' is unused}}
f8(1.0)         // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
f8(b: 1.0)         // expected-error {{extraneous argument label 'b:' in call}}


class CurriedClass {
  func method1() {}
  func method2(_ a: Int) -> (_ b : Int) -> () { return { b in () } }
  func method3(_ a: Int, b : Int) {}  // expected-note 5 {{'method3(_:b:)' declared here}}
}

let c = CurriedClass()
_ = c.method1
c.method1(1)         // expected-error {{argument passed to call that takes no arguments}}
_ = c.method2(1)
_ = c.method2(1.0)   // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
c.method2(1)(2)
c.method2(1)(c: 2)   // expected-error {{extraneous argument label 'c:' in call}}
c.method2(1)(c: 2.0) // expected-error {{extraneous argument label 'c:' in call}}
c.method2(1)(2.0) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
c.method2(1.0)(2) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
c.method2(1.0)(2.0) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}

CurriedClass.method1(c)()
_ = CurriedClass.method1(c)
CurriedClass.method1(c)(1)         // expected-error {{argument passed to call that takes no arguments}}
CurriedClass.method1(2.0)(1)       // expected-error {{instance member 'method1' cannot be used on type 'CurriedClass'; did you mean to use a value of this type instead?}}

CurriedClass.method2(c)(32)(b: 1) // expected-error{{extraneous argument label 'b:' in call}}
_ = CurriedClass.method2(c)
_ = CurriedClass.method2(c)(32)
_ = CurriedClass.method2(1,2)      // expected-error {{instance member 'method2' cannot be used on type 'CurriedClass'; did you mean to use a value of this type instead?}}
CurriedClass.method2(c)(1.0)(b: 1) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
CurriedClass.method2(c)(1)(1.0) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
CurriedClass.method2(c)(2)(c: 1.0) // expected-error {{extraneous argument label 'c:'}}

CurriedClass.method3(c)(32, b: 1)
_ = CurriedClass.method3(c)
_ = CurriedClass.method3(c)(1, 2)        // expected-error {{missing argument label 'b:' in call}} {{32-32=b: }}
_ = CurriedClass.method3(c)(1, b: 2)(32) // expected-error {{cannot call value of non-function type '()'}}
_ = CurriedClass.method3(1, 2)           // expected-error {{instance member 'method3' cannot be used on type 'CurriedClass'; did you mean to use a value of this type instead?}}
CurriedClass.method3(c)(1.0, b: 1)       // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
CurriedClass.method3(c)(1)               // expected-error {{missing argument for parameter 'b' in call}}
CurriedClass.method3(c)(c: 1.0)          // expected-error {{missing argument for parameter #1 in call}}


extension CurriedClass {
  func f() {
    method3(1, b: 2)
    method3()            // expected-error {{missing argument for parameter #1 in call}}
    method3(42)          // expected-error {{missing argument for parameter 'b' in call}}
    method3(self)        // expected-error {{missing argument for parameter 'b' in call}}
  }
}

extension CurriedClass {
  func m1(_ a : Int, b : Int) {}
  
  func m2(_ a : Int) {}
}

// <rdar://problem/23718816> QoI: "Extra argument" error when accidentally currying a method
CurriedClass.m1(2, b: 42)   // expected-error {{instance member 'm1' cannot be used on type 'CurriedClass'; did you mean to use a value of this type instead?}}


// <rdar://problem/22108559> QoI: Confusing error message when calling an instance method as a class method
CurriedClass.m2(12)  // expected-error {{instance member 'm2' cannot be used on type 'CurriedClass'; did you mean to use a value of this type instead?}}




// <rdar://problem/20491794> Error message does not tell me what the problem is
enum Color {
  case Red
  case Unknown(description: String)

  static func rainbow() -> Color {}
  
  static func overload(a : Int) -> Color {}
  static func overload(b : Int) -> Color {}
  
  static func frob(_ a : Int, b : inout Int) -> Color {}
}
let _: (Int, Color) = [1,2].map({ ($0, .Unknown("")) }) // expected-error {{'map' produces '[T]', not the expected contextual result type '(Int, Color)'}}

// FIXME: rdar://41416346
let _: [(Int, Color)] = [1,2].map({ ($0, .Unknown("")) })// expected-error {{'map' produces '[T]', not the expected contextual result type '[(Int, Color)]'}}

let _: [Color] = [1,2].map { _ in .Unknown("") }// expected-error {{missing argument label 'description:' in call}} {{44-44=description: }}

let _: (Int) -> (Int, Color) = { ($0, .Unknown("")) } // expected-error {{missing argument label 'description:' in call}} {{48-48=description: }}
let _: Color = .Unknown("") // expected-error {{missing argument label 'description:' in call}} {{25-25=description: }}
let _: Color = .Unknown // expected-error {{member 'Unknown' expects argument of type '(description: String)'}}
let _: Color = .Unknown(42) // expected-error {{missing argument label 'description:' in call}}
let _ : Color = .rainbow(42)  // expected-error {{argument passed to call that takes no arguments}}

let _ : (Int, Float) = (42.0, 12)  // expected-error {{cannot convert value of type 'Double' to specified type 'Int'}}

let _ : Color = .rainbow  // expected-error {{member 'rainbow' is a function; did you mean to call it?}} {{25-25=()}}

let _: Color = .overload(a : 1.0)  // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
let _: Color = .overload(1.0)  // expected-error {{ambiguous reference to member 'overload'}}
// expected-note @-1 {{overloads for 'overload' exist with these partially matching parameter lists: (a: Int), (b: Int)}}
let _: Color = .overload(1)  // expected-error {{ambiguous reference to member 'overload'}}
// expected-note @-1 {{overloads for 'overload' exist with these partially matching parameter lists: (a: Int), (b: Int)}}
let _: Color = .frob(1.0, &i) // expected-error {{missing argument label 'b:' in call}}
let _: Color = .frob(1.0, b: &i) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
let _: Color = .frob(1, i)  // expected-error {{missing argument label 'b:' in call}}
let _: Color = .frob(1, b: i)  // expected-error {{passing value of type 'Int' to an inout parameter requires explicit '&'}}
let _: Color = .frob(1, &d) // expected-error {{missing argument label 'b:' in call}}
let _: Color = .frob(1, b: &d) // expected-error {{cannot convert value of type 'Double' to expected argument type 'Int'}}
var someColor : Color = .red // expected-error {{enum type 'Color' has no case 'red'; did you mean 'Red'}}
someColor = .red  // expected-error {{enum type 'Color' has no case 'red'; did you mean 'Red'}}

func testTypeSugar(_ a : Int) {
  typealias Stride = Int

  let x = Stride(a)
  x+"foo"            // expected-error {{binary operator '+' cannot be applied to operands of type 'Stride' (aka 'Int') and 'String'}}
// expected-note @-1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (String, String)}}
}

// <rdar://problem/21974772> SegFault in FailureDiagnosis::visitInOutExpr
func r21974772(_ y : Int) {
  let x = &(1.0 + y) // expected-error {{use of extraneous '&'}}
}

// <rdar://problem/22020088> QoI: missing member diagnostic on optional gives worse error message than existential/bound generic/etc
protocol r22020088P {}

func r22020088Foo<T>(_ t: T) {}

func r22020088bar(_ p: r22020088P?) {
  r22020088Foo(p.fdafs)  // expected-error {{value of type 'r22020088P?' has no member 'fdafs'}}
}

// <rdar://problem/22288575> QoI: poor diagnostic involving closure, bad parameter label, and mismatch return type
func f(_ arguments: [String]) -> [ArraySlice<String>] {
  return arguments.split(maxSplits: 1, omittingEmptySubsequences: false, whereSeparator: { $0 == "--" })
}



struct AOpts : OptionSet {
  let rawValue : Int
}

class B {
  func function(_ x : Int8, a : AOpts) {}
  func f2(_ a : AOpts) {}
  static func f1(_ a : AOpts) {}
}

class GenClass<T> {}
struct GenStruct<T> {}
enum GenEnum<T> {}

func test(_ a : B) {
  B.f1(nil)              // expected-error {{'nil' is not compatible with expected argument type 'AOpts'}}
  a.function(42, a: nil) // expected-error {{'nil' is not compatible with expected argument type 'AOpts'}}
  a.function(42, nil)    // expected-error {{missing argument label 'a:' in call}}
  a.f2(nil)              // expected-error {{'nil' is not compatible with expected argument type 'AOpts'}}

  func foo1(_ arg: Bool) -> Int {return nil}
  func foo2<T>(_ arg: T) -> GenClass<T> {return nil}
  func foo3<T>(_ arg: T) -> GenStruct<T> {return nil}
  func foo4<T>(_ arg: T) -> GenEnum<T> {return nil}
  // expected-error@-4 {{'nil' is incompatible with return type 'Int'}}
  // expected-error@-4 {{'nil' is incompatible with return type 'GenClass<T>'}}
  // expected-error@-4 {{'nil' is incompatible with return type 'GenStruct<T>'}}
  // expected-error@-4 {{'nil' is incompatible with return type 'GenEnum<T>'}}

  let clsr1: () -> Int = {return nil}
  let clsr2: () -> GenClass<Bool> = {return nil}
  let clsr3: () -> GenStruct<String> = {return nil}
  let clsr4: () -> GenEnum<Double?> = {return nil}
  // expected-error@-4 {{'nil' is not compatible with closure result type 'Int'}}
  // expected-error@-4 {{'nil' is not compatible with closure result type 'GenClass<Bool>'}}
  // expected-error@-4 {{'nil' is not compatible with closure result type 'GenStruct<String>'}}
  // expected-error@-4 {{'nil' is not compatible with closure result type 'GenEnum<Double?>'}}

  var number = 0
  var genClassBool = GenClass<Bool>()
  var funcFoo1 = foo1

  number = nil
  genClassBool = nil
  funcFoo1 = nil
  // expected-error@-3 {{'nil' cannot be assigned to type 'Int'}}
  // expected-error@-3 {{'nil' cannot be assigned to type 'GenClass<Bool>'}}
  // expected-error@-3 {{'nil' cannot be assigned to type '(Bool) -> Int'}}
}

// <rdar://problem/21684487> QoI: invalid operator use inside a closure reported as a problem with the closure
typealias MyClosure = ([Int]) -> Bool
func r21684487() {
  var closures = Array<MyClosure>()
  let testClosure = {(list: [Int]) -> Bool in return true}
  
  let closureIndex = closures.index{$0 === testClosure} // expected-error {{cannot check reference equality of functions;}}
}

// <rdar://problem/18397777> QoI: special case comparisons with nil
func r18397777(_ d : r21447318?) {
  let c = r21447318()

  if c != nil { // expected-warning {{comparing non-optional value of type 'r21447318' to 'nil' always returns true}}
  }
  
  if d {  // expected-error {{optional type 'r21447318?' cannot be used as a boolean; test for '!= nil' instead}} {{6-6=(}} {{7-7= != nil)}}
  }
  
  if !d { // expected-error {{optional type 'r21447318?' cannot be used as a boolean; test for '!= nil' instead}} {{7-7=(}} {{8-8= != nil)}}

  }

  if !Optional(c) { // expected-error {{optional type 'Optional<r21447318>' cannot be used as a boolean; test for '!= nil' instead}} {{7-7=(}} {{18-18= != nil)}}
  }
}


// <rdar://problem/22255907> QoI: bad diagnostic if spurious & in argument list
func r22255907_1<T>(_ a : T, b : Int) {}
func r22255907_2<T>(_ x : Int, a : T, b: Int) {}

func reachabilityForInternetConnection() {
  var variable: Int = 42
  r22255907_1(&variable, b: 2.1) // expected-error {{'&' used with non-inout argument of type 'Int'}} {{15-16=}}
  r22255907_2(1, a: &variable, b: 2.1)// expected-error {{'&' used with non-inout argument of type 'Int'}} {{21-22=}}
}

// <rdar://problem/21601687> QoI: Using "=" instead of "==" in if statement leads to incorrect error message
if i = 6 { } // expected-error {{use of '=' in a boolean context, did you mean '=='?}} {{6-7===}}

_ = (i = 6) ? 42 : 57 // expected-error {{use of '=' in a boolean context, did you mean '=='?}} {{8-9===}}


// <rdar://problem/22263468> QoI: Not producing specific argument conversion diagnostic for tuple init
func r22263468(_ a : String?) {
  typealias MyTuple = (Int, String)
  _ = MyTuple(42, a) // expected-error {{value of optional type 'String?' must be unwrapped to a value of type 'String'}}
  // expected-note@-1{{coalesce using '??' to provide a default when the optional value contains 'nil'}}
  // expected-note@-2{{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
}


// rdar://22470302 - Crash with parenthesized call result.
class r22470302Class {
  func f() {}
}

func r22470302(_ c: r22470302Class) {
  print((c.f)(c))  // expected-error {{argument passed to call that takes no arguments}}
}



// <rdar://problem/21928143> QoI: Pointfree reference to generic initializer in generic context does not compile
extension String {
  @available(*, unavailable, message: "calling this is unwise")
  func unavail<T : Sequence> // expected-note 2 {{'unavail' has been explicitly marked unavailable here}}
    (_ a : T) -> String where T.Iterator.Element == String {}
}
extension Array {
  func g() -> String {
    return "foo".unavail([""])  // expected-error {{'unavail' is unavailable: calling this is unwise}}
  }
  
  func h() -> String {
    return "foo".unavail([0])  // expected-error {{'unavail' is unavailable: calling this is unwise}}
  }
}

// <rdar://problem/22519983> QoI: Weird error when failing to infer archetype
func safeAssign<T: RawRepresentable>(_ lhs: inout T) -> Bool {}
// expected-note @-1 {{in call to function 'safeAssign'}}
let a = safeAssign // expected-error {{generic parameter 'T' could not be inferred}}

// <rdar://problem/21692808> QoI: Incorrect 'add ()' fixit with trailing closure
struct Radar21692808<Element> {
  init(count: Int, value: Element) {}
}
func radar21692808() -> Radar21692808<Int> {
  return Radar21692808<Int>(count: 1) { // expected-error {{cannot invoke initializer for type 'Radar21692808<Int>' with an argument list of type '(count: Int, () -> Int)'}}
    // expected-note @-1 {{expected an argument list of type '(count: Int, value: Element)'}}
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
  a.0 = 42 // expected-error{{value of optional type '(Int, Int)?' must be unwrapped to refer to member '0' of wrapped base type '(Int, Int)'}}
  // expected-note@-1{{chain the optional }}
  // expected-note@-2{{force-unwrap using '!'}}
}

// <rdar://problem/21890157> QoI: wrong error message when accessing properties on optional structs without unwrapping
struct ExampleStruct21890157 {
  var property = "property"
}
var example21890157: ExampleStruct21890157?
example21890157.property = "confusing"  // expected-error {{value of optional type 'ExampleStruct21890157?' must be unwrapped to refer to member 'property' of wrapped base type 'ExampleStruct21890157'}}
  // expected-note@-1{{chain the optional }}
  // expected-note@-2{{force-unwrap using '!'}}


struct UnaryOp {}

_ = -UnaryOp() // expected-error {{unary operator '-' cannot be applied to an operand of type 'UnaryOp'}}
// expected-note @-1 {{overloads for '-' exist with these partially matching parameter lists: (Float), (Double)}}


// <rdar://problem/23433271> Swift compiler segfault in failure diagnosis
func f23433271(_ x : UnsafePointer<Int>) {}
func segfault23433271(_ a : UnsafeMutableRawPointer) {
  f23433271(a[0])  // expected-error {{value of type 'UnsafeMutableRawPointer' has no subscripts}}
}



// <rdar://problem/23272739> Poor diagnostic due to contextual constraint
func r23272739(_ contentType: String) {
  let actualAcceptableContentTypes: Set<String> = []
  return actualAcceptableContentTypes.contains(contentType)  // expected-error {{unexpected non-void return value in void function}}
}

// <rdar://problem/23641896> QoI: Strings in Swift cannot be indexed directly with integer offsets
func r23641896() {
  var g = "Hello World"
  g.replaceSubrange(0...2, with: "ce")  // expected-error {{cannot convert value of type 'ClosedRange<Int>' to expected argument type 'Range<String.Index>'}}

  _ = g[12]  // expected-error {{'subscript' is unavailable: cannot subscript String with an Int, see the documentation comment for discussion}}

}


// <rdar://problem/23718859> QoI: Incorrectly flattening ((Int,Int)) argument list to (Int,Int) when printing note
func test17875634() {
  var match: [(Int, Int)] = []
  var row = 1
  var col = 2
  
  match.append(row, col)  // expected-error {{instance method 'append' expects a single parameter of type '(Int, Int)'}} {{16-16=(}} {{24-24=)}}
}

// <https://github.com/apple/swift/pull/1205> Improved diagnostics for enums with associated values
enum AssocTest {
  case one(Int)
}

if AssocTest.one(1) == AssocTest.one(1) {} // expected-error{{binary operator '==' cannot be applied to two 'AssocTest' operands}}
// expected-note @-1 {{binary operator '==' cannot be synthesized for enums with associated values}}


// <rdar://problem/24251022> Swift 2: Bad Diagnostic Message When Adding Different Integer Types
func r24251022() {
  var a = 1
  var b: UInt32 = 2
  _ = a + b // expected-error {{unavailable}}
  a += a + // expected-error {{binary operator '+=' cannot be applied to operands of type 'Int' and 'UInt32'}} expected-note {{overloads for '+=' exist}}
    b
}

func overloadSetResultType(_ a : Int, b : Int) -> Int {
  // https://twitter.com/_jlfischer/status/712337382175952896
  // TODO: <rdar://problem/27391581> QoI: Nonsensical "binary operator '&&' cannot be applied to two 'Bool' operands"
  return a == b && 1 == 2  // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
}

postfix operator +++
postfix func +++ <T>(_: inout T) -> T { fatalError() } // expected-note {{in call to operator '+++'}}

// <rdar://problem/21523291> compiler error message for mutating immutable field is incorrect
func r21523291(_ bytes : UnsafeMutablePointer<UInt8>) {
  let i = 42

  // FIXME: rdar://41416382
  _ = bytes[i+++]  // expected-error {{generic parameter 'T' could not be inferred}}
}


// SR-1594: Wrong error description when using === on non-class types
class SR1594 {
  func sr1594(bytes : UnsafeMutablePointer<Int>, _ i : Int?) {
    _ = (i === nil) // expected-error {{value of type 'Int?' cannot be compared by reference; did you mean to compare by value?}} {{12-15===}}
    _ = (bytes === nil) // expected-error {{type 'UnsafeMutablePointer<Int>' is not optional, value can never be nil}}
    _ = (self === nil) // expected-warning {{comparing non-optional value of type 'AnyObject' to 'nil' always returns false}}
    _ = (i !== nil) // expected-error {{value of type 'Int?' cannot be compared by reference; did you mean to compare by value?}} {{12-15=!=}}
    _ = (bytes !== nil) // expected-error {{type 'UnsafeMutablePointer<Int>' is not optional, value can never be nil}}
    _ = (self !== nil) // expected-warning {{comparing non-optional value of type 'AnyObject' to 'nil' always returns true}}
  }
}

func nilComparison(i: Int, o: AnyObject) {
  _ = i == nil // expected-warning {{comparing non-optional value of type 'Int' to 'nil' always returns false}}
  _ = nil == i // expected-warning {{comparing non-optional value of type 'Int' to 'nil' always returns false}}
  _ = i != nil // expected-warning {{comparing non-optional value of type 'Int' to 'nil' always returns true}}
  _ = nil != i // expected-warning {{comparing non-optional value of type 'Int' to 'nil' always returns true}}
  
  // FIXME(integers): uncomment these tests once the < is no longer ambiguous
  // _ = i < nil  // _xpected-error {{type 'Int' is not optional, value can never be nil}}
  // _ = nil < i  // _xpected-error {{type 'Int' is not optional, value can never be nil}}
  // _ = i <= nil // _xpected-error {{type 'Int' is not optional, value can never be nil}}
  // _ = nil <= i // _xpected-error {{type 'Int' is not optional, value can never be nil}}
  // _ = i > nil  // _xpected-error {{type 'Int' is not optional, value can never be nil}}
  // _ = nil > i  // _xpected-error {{type 'Int' is not optional, value can never be nil}}
  // _ = i >= nil // _xpected-error {{type 'Int' is not optional, value can never be nil}}
  // _ = nil >= i // _xpected-error {{type 'Int' is not optional, value can never be nil}}

  _ = o === nil // expected-warning {{comparing non-optional value of type 'AnyObject' to 'nil' always returns false}}
  _ = o !== nil // expected-warning {{comparing non-optional value of type 'AnyObject' to 'nil' always returns true}}
}

func secondArgumentNotLabeled(a: Int, _ b: Int) { }
secondArgumentNotLabeled(10, 20)
// expected-error@-1 {{missing argument label 'a:' in call}}

// <rdar://problem/23709100> QoI: incorrect ambiguity error due to implicit conversion
func testImplConversion(a : Float?) -> Bool {}
func testImplConversion(a : Int?) -> Bool {
  let someInt = 42
  let a : Int = testImplConversion(someInt) // expected-error {{argument labels '(_:)' do not match any available overloads}}
  // expected-note @-1 {{overloads for 'testImplConversion' exist with these partially matching parameter lists: (a: Float?), (a: Int?)}}
}

// <rdar://problem/23752537> QoI: Bogus error message: Binary operator '&&' cannot be applied to two 'Bool' operands
class Foo23752537 {
  var title: String?
  var message: String?
}

extension Foo23752537 {
  func isEquivalent(other: Foo23752537) {
    // TODO: <rdar://problem/27391581> QoI: Nonsensical "binary operator '&&' cannot be applied to two 'Bool' operands"
    // expected-error @+1 {{unexpected non-void return value in void function}}
    return (self.title != other.title && self.message != other.message)
  }
}

// <rdar://problem/27391581> QoI: Nonsensical "binary operator '&&' cannot be applied to two 'Bool' operands"
func rdar27391581(_ a : Int, b : Int) -> Int {
  return a == b && b != 0
  // expected-error @-1 {{cannot convert return expression of type 'Bool' to return type 'Int'}}
}

// <rdar://problem/22276040> QoI: not great error message with "withUnsafePointer" sametype constraints
func read2(_ p: UnsafeMutableRawPointer, maxLength: Int) {}
func read<T : BinaryInteger>() -> T? {
  var buffer : T 
  let n = withUnsafeMutablePointer(to: &buffer) { (p) in
    read2(UnsafePointer(p), maxLength: MemoryLayout<T>.size) // expected-error {{cannot convert value of type 'UnsafePointer<_>' to expected argument type 'UnsafeMutableRawPointer'}}
  }
}

func f23213302() {
  var s = Set<Int>()
  s.subtract(1) // expected-error {{cannot convert value of type 'Int' to expected argument type 'Set<Int>'}}
}

// <rdar://problem/24202058> QoI: Return of call to overloaded function in void-return context
func rdar24202058(a : Int) {
  return a <= 480 // expected-error {{unexpected non-void return value in void function}}
}

// SR-1752: Warning about unused result with ternary operator

struct SR1752 {
  func foo() {}
}

let sr1752: SR1752?

true ? nil : sr1752?.foo() // don't generate a warning about unused result since foo returns Void

// <rdar://problem/27891805> QoI: FailureDiagnosis doesn't look through 'try'
struct rdar27891805 {
  init(contentsOf: String, encoding: String) throws {}
  init(contentsOf: String, usedEncoding: inout String) throws {}
  init<T>(_ t: T) {}
}

try rdar27891805(contentsOfURL: nil, usedEncoding: nil)
// expected-error@-1 {{argument labels '(contentsOfURL:, usedEncoding:)' do not match any available overloads}}
// expected-note@-2 {{overloads for 'rdar27891805' exist with these partially matching parameter lists: (contentsOf: String, encoding: String), (contentsOf: String, usedEncoding: inout String)}}

// Make sure RawRepresentable fix-its don't crash in the presence of type variables
class NSCache<K, V> {
  func object(forKey: K) -> V? {}
}

class CacheValue {
  func value(x: Int) -> Int {} // expected-note {{found this candidate}}
  func value(y: String) -> String {} // expected-note {{found this candidate}}
}

func valueForKey<K>(_ key: K) -> CacheValue? {
  let cache = NSCache<K, CacheValue>()
  return cache.object(forKey: key)?.value // expected-error {{ambiguous reference to member 'value(x:)'}}
}

// SR-2242: poor diagnostic when argument label is omitted

func r27212391(x: Int, _ y: Int) {
  let _: Int = x + y
}

func r27212391(a: Int, x: Int, _ y: Int) {
  let _: Int = a + x + y
}

r27212391(3, 5)             // expected-error {{missing argument label 'x:' in call}}
r27212391(3, y: 5)          // expected-error {{incorrect argument labels in call (have '_:y:', expected 'x:_:')}}
r27212391(3, x: 5)          // expected-error {{argument 'x' must precede unnamed argument #1}} {{11-11=x: 5, }} {{12-18=}}
r27212391(y: 3, x: 5)       // expected-error {{incorrect argument labels in call (have 'y:x:', expected 'x:_:')}} {{11-12=x}} {{17-20=}}
r27212391(y: 3, 5)          // expected-error {{incorrect argument label in call (have 'y:_:', expected 'x:_:')}}
r27212391(x: 3, x: 5)       // expected-error {{extraneous argument label 'x:' in call}}
r27212391(a: 1, 3, y: 5)    // expected-error {{incorrect argument labels in call (have 'a:_:y:', expected 'a:x:_:')}}
r27212391(1, x: 3, y: 5)    // expected-error {{incorrect argument labels in call (have '_:x:y:', expected 'a:x:_:')}}
r27212391(a: 1, y: 3, x: 5) // expected-error {{incorrect argument labels in call (have 'a:y:x:', expected 'a:x:_:')}} {{17-18=x}} {{23-26=}}
r27212391(a: 1, 3, x: 5)    // expected-error {{argument 'x' must precede unnamed argument #2}} {{17-17=x: 5, }} {{18-24=}}

// SR-1255
func foo1255_1() {
  return true || false // expected-error {{unexpected non-void return value in void function}}
}
func foo1255_2() -> Int {
  return true || false // expected-error {{cannot convert return expression of type 'Bool' to return type 'Int'}}
}

// Diagnostic message for initialization with binary operations as right side
let foo1255_3: String = 1 + 2 + 3 // expected-error {{cannot convert value of type 'Int' to specified type 'String'}}
let foo1255_4: Dictionary<String, String> = ["hello": 1 + 2] // expected-error {{cannot convert value of type 'Int' to expected dictionary value type 'String'}}
let foo1255_5: Dictionary<String, String> = [(1 + 2): "world"] // expected-error {{cannot convert value of type 'Int' to expected dictionary key type 'String'}}
let foo1255_6: [String] = [1 + 2 + 3] // expected-error {{cannot convert value of type 'Int' to expected element type 'String'}}

// SR-2208
struct Foo2208 {
  func bar(value: UInt) {}
}

func test2208() {
  let foo = Foo2208()
  let a: Int = 1
  let b: Int = 2
  let result = a / b
  foo.bar(value: a / b)  // expected-error {{cannot convert value of type 'Int' to expected argument type 'UInt'}}
  foo.bar(value: result) // expected-error {{cannot convert value of type 'Int' to expected argument type 'UInt'}}
  foo.bar(value: UInt(result)) // Ok
}

// SR-2164: Erroneous diagnostic when unable to infer generic type

struct SR_2164<A, B> { // expected-note 3 {{'B' declared as parameter to type 'SR_2164'}} expected-note 2 {{'A' declared as parameter to type 'SR_2164'}} expected-note * {{generic type 'SR_2164' declared here}}
  init(a: A) {}
  init(b: B) {}
  init(c: Int) {}
  init(_ d: A) {}
  init(e: A?) {}
}

struct SR_2164_Array<A, B> { // expected-note {{'B' declared as parameter to type 'SR_2164_Array'}} expected-note * {{generic type 'SR_2164_Array' declared here}}
  init(_ a: [A]) {}
}

struct SR_2164_Dict<A: Hashable, B> { // expected-note {{'B' declared as parameter to type 'SR_2164_Dict'}} expected-note * {{generic type 'SR_2164_Dict' declared here}}
  init(a: [A: Double]) {}
}

SR_2164(a: 0) // expected-error {{generic parameter 'B' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
SR_2164(b: 1) // expected-error {{generic parameter 'A' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
SR_2164(c: 2) // expected-error {{generic parameter 'A' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
SR_2164(3) // expected-error {{generic parameter 'B' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
SR_2164_Array([4]) // expected-error {{generic parameter 'B' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
SR_2164(e: 5) // expected-error {{generic parameter 'B' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
SR_2164_Dict(a: ["pi": 3.14]) // expected-error {{generic parameter 'B' could not be inferred}} expected-note {{explicitly specify the generic arguments to fix this issue}}
SR_2164<Int>(a: 0) // expected-error {{generic type 'SR_2164' specialized with too few type parameters (got 1, but expected 2)}}
SR_2164<Int>(b: 1) // expected-error {{generic type 'SR_2164' specialized with too few type parameters (got 1, but expected 2)}}
let _ = SR_2164<Int, Bool>(a: 0)    // Ok
let _ = SR_2164<Int, Bool>(b: true) // Ok
SR_2164<Int, Bool, Float>(a: 0) // expected-error {{generic type 'SR_2164' specialized with too many type parameters (got 3, but expected 2)}}
SR_2164<Int, Bool, Float>(b: 0) // expected-error {{generic type 'SR_2164' specialized with too many type parameters (got 3, but expected 2)}}

// <rdar://problem/29850459> Swift compiler misreports type error in ternary expression

let r29850459_flag = true
let r29850459_a: Int = 0
let r29850459_b: Int = 1
func r29850459() -> Bool { return false }
let _ = (r29850459_flag ? r29850459_a : r29850459_b) + 42.0 // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'Double'}}
// expected-note@-1 {{overloads for '+' exist with these partially matching parameter lists: (Double, Double), (Int, Int)}}
let _ = ({ true }() ? r29850459_a : r29850459_b) + 42.0 // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'Double'}}
// expected-note@-1 {{overloads for '+' exist with these partially matching parameter lists: (Double, Double), (Int, Int)}}
let _ = (r29850459() ? r29850459_a : r29850459_b) + 42.0 // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'Double'}}
// expected-note@-1 {{overloads for '+' exist with these partially matching parameter lists: (Double, Double), (Int, Int)}}
let _ = ((r29850459_flag || r29850459()) ? r29850459_a : r29850459_b) + 42.0 // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'Double'}}
// expected-note@-1 {{overloads for '+' exist with these partially matching parameter lists: (Double, Double), (Int, Int)}}

// SR-6272: Tailored diagnostics with fixits for numerical conversions

func SR_6272_a() {
  enum Foo: Int {
    case bar
  }

  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Int' and 'Float'}} {{35-35=Int(}} {{42-42=)}}
  // expected-note@+1 {{expected an argument list of type '(Int, Int)'}}
  let _: Int = Foo.bar.rawValue * Float(0)

  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Int' and 'Float'}} {{18-18=Float(}} {{34-34=)}}
  // expected-note@+1 {{expected an argument list of type '(Float, Float)'}}
  let _: Float = Foo.bar.rawValue * Float(0)

  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Int' and 'Float'}} {{none}}
  // expected-note@+1 {{overloads for '*' exist with these partially matching parameter lists: (Float, Float), (Int, Int)}}
  Foo.bar.rawValue * Float(0)
}

func SR_6272_b() {
  let lhs = Float(3)
  let rhs = Int(0)

  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Float' and 'Int'}} {{24-24=Float(}} {{27-27=)}}
  // expected-note@+1 {{expected an argument list of type '(Float, Float)'}}
  let _: Float = lhs * rhs

  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Float' and 'Int'}} {{16-16=Int(}} {{19-19=)}}
  // expected-note@+1 {{expected an argument list of type '(Int, Int)'}}
  let _: Int = lhs * rhs

  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Float' and 'Int'}} {{none}}
  // expected-note@+1 {{overloads for '*' exist with these partially matching parameter lists: (Float, Float), (Int, Int)}}
  lhs * rhs
}

func SR_6272_c() {
  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Int' and 'String'}} {{none}}
  // expected-note@+1 {{expected an argument list of type '(Int, Int)'}}
  Int(3) * "0"

  struct S {}
  // expected-error@+2 {{binary operator '*' cannot be applied to operands of type 'Int' and 'S'}} {{none}}
  // expected-note@+1 {{expected an argument list of type '(Int, Int)'}}
  Int(10) * S()
}

struct SR_6272_D: ExpressibleByIntegerLiteral {
  typealias IntegerLiteralType = Int
  init(integerLiteral: Int) {}
  static func +(lhs: SR_6272_D, rhs: Int) -> Float { return 42.0 } // expected-note {{found this candidate}}
}

func SR_6272_d() {
  let x: Float = 1.0

  // expected-error@+2 {{binary operator '+' cannot be applied to operands of type 'SR_6272_D' and 'Float'}} {{none}}
  // expected-note@+1 {{overloads for '+' exist with these partially matching parameter lists: (SR_6272_D, Int), (Float, Float)}}
  let _: Float = SR_6272_D(integerLiteral: 42) + x

  // expected-error@+2 {{binary operator '+' cannot be applied to operands of type 'SR_6272_D' and 'Double'}} {{none}}
  // expected-note@+1 {{overloads for '+' exist with these partially matching parameter lists: (SR_6272_D, Int), (Double, Double)}}
  let _: Float = SR_6272_D(integerLiteral: 42) + 42.0

  // expected-error@+2 {{binary operator '+' cannot be applied to operands of type 'SR_6272_D' and 'Float'}} {{none}}
  // expected-note@+1 {{overloads for '+' exist with these partially matching parameter lists: (SR_6272_D, Int), (Float, Float)}}
  let _: Float = SR_6272_D(integerLiteral: 42) + x + 1.0
}

// Ambiguous overload inside a trailing closure

func ambiguousCall() -> Int {} // expected-note {{found this candidate}}
func ambiguousCall() -> Float {} // expected-note {{found this candidate}}

func takesClosure(fn: () -> ()) {}

takesClosure() { ambiguousCall() } // expected-error {{ambiguous use of 'ambiguousCall()'}}

// SR-4692: Useless diagnostics calling non-static method

class SR_4692_a {
  private static func foo(x: Int, y: Bool) {
    self.bar(x: x)
    // expected-error@-1 {{instance member 'bar' cannot be used on type 'SR_4692_a'}}
  }

  private func bar(x: Int) {
  }
}

class SR_4692_b {
  static func a() {
    self.f(x: 3, y: true)
    // expected-error@-1 {{instance member 'f' cannot be used on type 'SR_4692_b'}}
  }

  private func f(a: Int, b: Bool, c: String) {
    self.f(x: a, y: b)
  }

  private func f(x: Int, y: Bool) {
  }
}

// rdar://problem/32101765 - Keypath diagnostics are not actionable/helpful

struct R32101765 { let prop32101765 = 0 }
let _: KeyPath<R32101765, Float> = \.prop32101765
// expected-error@-1 {{key path value type 'Int' cannot be converted to contextual type 'Float'}}
let _: KeyPath<R32101765, Float> = \R32101765.prop32101765
// expected-error@-1 {{key path value type 'Int' cannot be converted to contextual type 'Float'}}
let _: KeyPath<R32101765, Float> = \.prop32101765.unknown
// expected-error@-1 {{type 'Int' has no member 'unknown'}}
let _: KeyPath<R32101765, Float> = \R32101765.prop32101765.unknown
// expected-error@-1 {{type 'Int' has no member 'unknown'}}

// rdar://problem/32390726 - Bad Diagnostic: Don't suggest `var` to `let` when binding inside for-statement
for var i in 0..<10 { // expected-warning {{variable 'i' was never mutated; consider changing to 'let' constant}} {{5-9=}}
  _ = i + 1
}

// rdar://problem/32726044 - shrink reduced domains too far

public protocol P_32726044 {}

extension Int: P_32726044 {}
extension Float: P_32726044 {}

public func *(lhs: P_32726044, rhs: P_32726044) -> Double {
  fatalError()
}

func rdar32726044() -> Float {
  var f: Float = 0
  f = Float(1) * 100 // Ok
  let _: Float = Float(42) + 0 // Ok
  return f
}

// SR-5045 - Attempting to return result of reduce(_:_:) in a method with no return produces ambiguous error
func sr5045() {
  let doubles: [Double] = [1, 2, 3]
  return doubles.reduce(0, +)
  // expected-error@-1 {{unexpected non-void return value in void function}}
}

// rdar://problem/32934129 - QoI: misleading diagnostic
class L_32934129<T : Comparable> {
  init(_ value: T) { self.value = value }
  init(_ value: T, _ next: L_32934129<T>?) {
    self.value = value
    self.next = next
  }

  var value: T
  var next: L_32934129<T>? = nil

  func length() -> Int {
    func inner(_ list: L_32934129<T>?, _ count: Int) {
    guard let list = list else { return count } // expected-error {{unexpected non-void return value in void function}}
      return inner(list.next, count + 1)
    }

    return inner(self, 0) // expected-error {{cannot convert return expression of type '()' to return type 'Int'}}
  }
}

// rdar://problem/31671195 - QoI: erroneous diagnostic - cannot call value of non-function type

class C_31671195 {
  var name: Int { fatalError() }
  func name(_: Int) { fatalError() }
}
C_31671195().name(UInt(0))
// expected-error@-1 {{cannot convert value of type 'UInt' to expected argument type 'Int'}}


// rdar://problem/28456467 - QoI: erroneous diagnostic - cannot call value of non-function type

class AST_28456467 {
  var hasStateDef: Bool { return false }
}

protocol Expr_28456467 {}

class ListExpr_28456467 : AST_28456467, Expr_28456467 {
  let elems: [Expr_28456467]

  init(_ elems:[Expr_28456467] ) {
    self.elems = elems
  }

  override var hasStateDef: Bool {
    return elems.first(where: { $0.hasStateDef }) != nil
    // expected-error@-1 {{value of type 'Expr_28456467' has no member 'hasStateDef'}}
  }
}

// rdar://problem/31849281 - Let's play "bump the argument"

struct rdar31849281 { var foo, a, b, c: Int }
_ = rdar31849281(a: 101, b: 102, c: 103, foo: 104) // expected-error {{argument 'foo' must precede argument 'a'}} {{18-18=foo: 104, }} {{40-50=}}

_ = rdar31849281(a: 101, c: 103, b: 102, foo: 104) // expected-error {{argument 'foo' must precede argument 'a'}} {{18-18=foo: 104, }} {{40-50=}}
_ = rdar31849281(foo: 104, a: 101, c: 103, b: 102) // expected-error {{argument 'b' must precede argument 'c'}} {{36-36=b: 102, }} {{42-50=}}

_ = rdar31849281(b: 102, c: 103, a: 101, foo: 104) // expected-error {{incorrect argument labels in call (have 'b:c:a:foo:', expected 'foo:a:b:c:')}} {{18-19=foo}} {{26-27=a}} {{34-35=b}} {{42-45=c}}
_ = rdar31849281(foo: 104, b: 102, c: 103, a: 101) // expected-error {{argument 'a' must precede argument 'b'}} {{28-28=a: 101, }} {{42-50=}}

func var_31849281(_ a: Int, _ b: Int..., c: Int) {}
var_31849281(1, c: 10, 3, 4, 5, 6, 7, 8, 9) // expected-error {{unnamed argument #3 must precede argument 'c'}} {{17-17=3, 4, 5, 6, 7, 8, 9, }} {{22-43=}}

func fun_31849281(a: (Bool) -> Bool, b: (Int) -> (String), c: [Int?]) {}
fun_31849281(c: [nil, 42], a: { !$0 }, b: { (num: Int) -> String in return "\(num)" })
// expected-error @-1 {{incorrect argument labels in call (have 'c:a:b:', expected 'a:b:c:')}} {{14-15=a}} {{28-29=b}} {{40-41=c}}
fun_31849281(a: { !$0 }, c: [nil, 42], b: { (num: Int) -> String in return String(describing: num) })
// expected-error @-1 {{argument 'b' must precede argument 'c'}} {{26-26=b: { (num: Int) -> String in return String(describing: num) }, }} {{38-101=}}
fun_31849281(a: { !$0 }, c: [nil, 42], b: { "\($0)" })
// expected-error @-1 {{argument 'b' must precede argument 'c'}} {{26-26=b: { "\\($0)" }, }} {{38-54=}}

func f_31849281(x: Int, y: Int, z: Int) {}
f_31849281(42, y: 10, x: 20) // expected-error {{incorrect argument labels in call (have '_:y:x:', expected 'x:y:z:')}} {{12-12=x: }} {{23-24=z}}

func sr5081() {
  var a = ["1", "2", "3", "4", "5"]
  var b = [String]()
  b = a[2...4] // expected-error {{cannot assign value of type 'ArraySlice<String>' to type '[String]'}}
}

func rdar17170728() {
  var i: Int? = 1
  var j: Int?
  var k: Int? = 2

  let _ = [i, j, k].reduce(0 as Int?) {
    $0 && $1 ? $0! + $1! : ($0 ? $0! : ($1 ? $1! : nil))
    // expected-error@-1 {{ambiguous use of operator '+'}}
  }
}

// https://bugs.swift.org/browse/SR-5934 - failure to emit diagnostic for bad
// generic constraints
func elephant<T, U>(_: T) where T : Collection, T.Element == U, T.Element : Hashable {}
// expected-note@-1 {{in call to function 'elephant'}}

func platypus<T>(a: [T]) {
    _ = elephant(a) // expected-error {{generic parameter 'U' could not be inferred}}
}

// Another case of the above.
func badTypes() {
  let sequence:AnySequence<[Int]> = AnySequence() { AnyIterator() { [3] }}
  let array = [Int](sequence)
  // expected-error@-1 {{type of expression is ambiguous without more context}}
  // FIXME: terrible diagnostic
}

// rdar://34357545
func unresolvedTypeExistential() -> Bool {
  return (Int.self==_{})
  // expected-error@-1 {{ambiguous reference to member '=='}}
}
