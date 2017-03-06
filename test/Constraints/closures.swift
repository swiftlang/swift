// RUN: %target-typecheck-verify-swift

func myMap<T1, T2>(_ array: [T1], _ fn: (T1) -> T2) -> [T2] {}

var intArray : [Int]

_ = myMap(intArray, { String($0) })
_ = myMap(intArray, { x -> String in String(x) } )

// Closures with too few parameters.
func foo(_ x: (Int, Int) -> Int) {}
foo({$0}) // expected-error{{contextual closure type '(Int, Int) -> Int' expects 2 arguments, but 1 was used in closure body}}

struct X {}
func mySort(_ array: [String], _ predicate: (String, String) -> Bool) -> [String] {}
func mySort(_ array: [X], _ predicate: (X, X) -> Bool) -> [X] {}
var strings : [String]
_ = mySort(strings, { x, y in x < y })

// Closures with inout arguments.
func f0<T, U>(_ t: T, _ f: (inout T) -> U) -> U {
  var t2 = t
  return f(&t2)
}

struct X2 {
  func g() -> Float { return 0 }  
}

_ = f0(X2(), {$0.g()})

// Autoclosure
func f1(f: @autoclosure () -> Int) { }
func f2() -> Int { }
f1(f: f2) // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}{{9-9=()}}
f1(f: 5)

// Ternary in closure
var evenOrOdd : (Int) -> String = {$0 % 2 == 0 ? "even" : "odd"}

// <rdar://problem/15367882>
func foo() {
  not_declared({ $0 + 1 }) // expected-error{{use of unresolved identifier 'not_declared'}}
}

// <rdar://problem/15536725>
struct X3<T> {
  init(_: (T) -> ()) {}
}

func testX3(_ x: Int) {
  var x = x
  _ = X3({ x = $0 })
  _ = x
}

// <rdar://problem/13811882>
func test13811882() {
  var _ : (Int) -> (Int, Int) = {($0, $0)}
  var x = 1
  var _ : (Int) -> (Int, Int) = {($0, x)}
  x = 2
}


// <rdar://problem/21544303> QoI: "Unexpected trailing closure" should have a fixit to insert a 'do' statement
// <https://bugs.swift.org/browse/SR-3671>
func r21544303() {
  var inSubcall = true
  {
  }  // expected-error {{computed property must have accessors specified}}
  inSubcall = false

  // This is a problem, but isn't clear what was intended.
  var somethingElse = true {
  }  // expected-error {{computed property must have accessors specified}}
  inSubcall = false

  var v2 : Bool = false
  v2 = inSubcall
  {  // expected-error {{cannot call value of non-function type 'Bool'}} expected-note {{did you mean to use a 'do' statement?}} {{3-3=do }}
  }
}


// <https://bugs.swift.org/browse/SR-3671>
func SR3671() {
  let n = 42
  func consume(_ x: Int) {}

  { consume($0) }(42)
  ;

  ({ $0(42) } { consume($0) }) // expected-note {{callee is here}}

  { print(42) }  // expected-warning {{braces here form a trailing closure separated from its callee by multiple newlines}} expected-note {{did you mean to use a 'do' statement?}} {{3-3=do }} expected-error {{cannot call value of non-function type '()'}}
  ;

  ({ $0(42) } { consume($0) }) // expected-note {{callee is here}}

  { print($0) }  // expected-warning {{braces here form a trailing closure separated from its callee by multiple newlines}} expected-error {{cannot call value of non-function type '()'}}
  ;

  ({ $0(42) } { consume($0) }) // expected-note {{callee is here}}

  { [n] in print(42) }  // expected-warning {{braces here form a trailing closure separated from its callee by multiple newlines}} expected-error {{cannot call value of non-function type '()'}}
  ;

  ({ $0(42) } { consume($0) }) // expected-note {{callee is here}}

  { consume($0) }(42)  // expected-warning {{braces here form a trailing closure separated from its callee by multiple newlines}} expected-error {{cannot call value of non-function type '()'}}
  ;

  ({ $0(42) } { consume($0) }) // expected-note {{callee is here}}

  { (x: Int) in consume(x) }(42)  // expected-warning {{braces here form a trailing closure separated from its callee by multiple newlines}} expected-error {{cannot call value of non-function type '()'}}
  ;

  // This is technically a valid call, so nothing goes wrong until (42)

  { $0(3) }
  { consume($0) }(42)  // expected-error {{cannot call value of non-function type '()'}}
  ;
  ({ $0(42) })
  { consume($0) }(42)  // expected-error {{cannot call value of non-function type '()'}}
  ;
  { $0(3) }
  { [n] in consume($0) }(42)  // expected-error {{cannot call value of non-function type '()'}}
  ;
  ({ $0(42) })
  { [n] in consume($0) }(42)  // expected-error {{cannot call value of non-function type '()'}}
  ;

  // Equivalent but more obviously unintended.

  { $0(3) }  // expected-note {{callee is here}}

  { consume($0) }(42)  // expected-error {{cannot call value of non-function type '()'}}
  // expected-warning@-1 {{braces here form a trailing closure separated from its callee by multiple newlines}}

  ({ $0(3) })  // expected-note {{callee is here}}

  { consume($0) }(42)  // expected-error {{cannot call value of non-function type '()'}}
  // expected-warning@-1 {{braces here form a trailing closure separated from its callee by multiple newlines}}
  ;

  // Also a valid call (!!)
  { $0 { $0 } } { $0 { 1 } }  // expected-error {{expression resolves to an unused function}}
  consume(111)
}


// <rdar://problem/22162441> Crash from failing to diagnose nonexistent method access inside closure
func r22162441(_ lines: [String]) {
  _ = lines.map { line in line.fooBar() }  // expected-error {{value of type 'String' has no member 'fooBar'}}
  _ = lines.map { $0.fooBar() }  // expected-error {{value of type 'String' has no member 'fooBar'}}
}


func testMap() {
  let a = 42
  [1,a].map { $0 + 1.0 } // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'Double'}}
  // expected-note @-1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (Double, Double), (Int, UnsafeMutablePointer<Pointee>), (Int, UnsafePointer<Pointee>)}}
}

// <rdar://problem/22414757> "UnresolvedDot" "in wrong phase" assertion from verifier
[].reduce { $0 + $1 }  // expected-error {{missing argument for parameter #1 in call}}




// <rdar://problem/22333281> QoI: improve diagnostic when contextual type of closure disagrees with arguments
var _: () -> Int = {0}

// expected-error @+1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{24-24=_ in }}
var _: (Int) -> Int = {0}

// expected-error @+1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}} {{24-24= _ in}}
var _: (Int) -> Int = { 0 }

// expected-error @+1 {{contextual type for closure argument list expects 2 arguments, which cannot be implicitly ignored}} {{29-29=_,_ in }}
var _: (Int, Int) -> Int = {0}

// expected-error @+1 {{contextual closure type '(Int, Int) -> Int' expects 2 arguments, but 3 were used in closure body}}
var _: (Int,Int) -> Int = {$0+$1+$2}

// expected-error @+1 {{contextual closure type '(Int, Int, Int) -> Int' expects 3 arguments, but 2 were used in closure body}}
var _: (Int, Int, Int) -> Int = {$0+$1}


var _: () -> Int = {a in 0}

// expected-error @+1 {{contextual closure type '(Int) -> Int' expects 1 argument, but 2 were used in closure body}}
var _: (Int) -> Int = {a,b in 0}

// expected-error @+1 {{contextual closure type '(Int) -> Int' expects 1 argument, but 3 were used in closure body}}
var _: (Int) -> Int = {a,b,c in 0}

var _: (Int, Int) -> Int = {a in 0}

// expected-error @+1 {{contextual closure type '(Int, Int, Int) -> Int' expects 3 arguments, but 2 were used in closure body}}
var _: (Int, Int, Int) -> Int = {a, b in a+b}

// <rdar://problem/15998821> Fail to infer types for closure that takes an inout argument
func r15998821() {
  func take_closure(_ x : (inout Int) -> ()) { }

  func test1() {
    take_closure { (a : inout Int) in
      a = 42
    }
  }

  func test2() {
    take_closure { a in
      a = 42
    }
  }

  func withPtr(_ body: (inout Int) -> Int) {}
  func f() { withPtr { p in return p } }

  let g = { x in x = 3 }
  take_closure(g)
}

// <rdar://problem/22602657> better diagnostics for closures w/o "in" clause
var _: (Int,Int) -> Int = {$0+$1+$2}  // expected-error {{contextual closure type '(Int, Int) -> Int' expects 2 arguments, but 3 were used in closure body}}


// Crash when re-typechecking bodies of non-single expression closures

struct CC {}
func callCC<U>(_ f: (CC) -> U) -> () {}

func typeCheckMultiStmtClosureCrash() {
  callCC { // expected-error {{unable to infer complex closure return type; add explicit type to disambiguate}} {{11-11= () -> Int in }}
    _ = $0
    return 1
  }
}

// SR-832 - both these should be ok
func someFunc(_ foo: ((String) -> String)?,
              bar: @escaping (String) -> String) {
    let _: (String) -> String = foo != nil ? foo! : bar
    let _: (String) -> String = foo ?? bar
}


// SR-1069 - Error diagnostic refers to wrong argument
class SR1069_W<T> {
  func append<Key: AnyObject>(value: T, forKey key: Key) where Key: Hashable {}
}
class SR1069_C<T> { let w: SR1069_W<(AnyObject, T) -> ()> = SR1069_W() }
struct S<T> {
  let cs: [SR1069_C<T>] = []

  func subscribe<Object: AnyObject>(object: Object?, method: (Object, T) -> ()) where Object: Hashable {
    let wrappedMethod = { (object: AnyObject, value: T) in }
    // expected-error @+1 {{value of optional type 'Object?' not unwrapped; did you mean to use '!' or '?'?}}
    cs.forEach { $0.w.append(value: wrappedMethod, forKey: object) }
  }
}

// Make sure we cannot infer an () argument from an empty parameter list.
func acceptNothingToInt (_: () -> Int) {}
func testAcceptNothingToInt(ac1: @autoclosure () -> Int) {
  acceptNothingToInt({ac1($0)})
  // expected-error@-1{{contextual closure type '() -> Int' expects 0 arguments, but 1 was used in closure body}}
}

// <rdar://problem/23570873> QoI: Poor error calling map without being able to infer "U" (closure result inference)
struct Thing {
  init?() {}
}
// This throws a compiler error
let things = Thing().map { thing in  // expected-error {{unable to infer complex closure return type; add explicit type to disambiguate}} {{34-34=-> (Thing) }}
  // Commenting out this makes it compile
  _ = thing
  return thing
}


// <rdar://problem/21675896> QoI: [Closure return type inference] Swift cannot find members for the result of inlined lambdas with branches
func r21675896(file : String) {
  let x: String = { // expected-error {{unable to infer complex closure return type; add explicit type to disambiguate}} {{20-20= () -> String in }}
    if true {
      return "foo"
    }
    else {
      return file
    }
  }().pathExtension
}




// <rdar://problem/19870975> Incorrect diagnostic for failed member lookups within closures passed as arguments ("(_) -> _")
func ident<T>(_ t: T) -> T {}
var c = ident({1.DOESNT_EXIST}) // error: expected-error {{value of type 'Int' has no member 'DOESNT_EXIST'}}

// <rdar://problem/20712541> QoI: Int/UInt mismatch produces useless error inside a block
var afterMessageCount : Int?

func uintFunc() -> UInt {}
func takeVoidVoidFn(_ a : () -> ()) {}
takeVoidVoidFn { () -> Void in
  afterMessageCount = uintFunc()  // expected-error {{cannot assign value of type 'UInt' to type 'Int?'}}
}

// <rdar://problem/19997471> Swift: Incorrect compile error when calling a function inside a closure
func f19997471(_ x: String) {}
func f19997471(_ x: Int) {}

func someGeneric19997471<T>(_ x: T) {
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
[0].map {  // expected-error {{unable to infer complex closure return type; add explicit type to disambiguate}} {{5-5=-> Int }}
  _ in
  let r =  (1,2).0
  return r
}


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
func foo20868864(_ callback: ([String]) -> ()) { }
func rdar20868864(_ s: String) {
  var s = s
  foo20868864 { (strings: [String]) in
    s = strings   // expected-error {{cannot assign value of type '[String]' to type 'String'}}
  }
}

// <rdar://problem/22058555> crash in cs diags in withCString
func r22058555() {
  var firstChar: UInt8 = 0
  "abc".withCString { chars in
    firstChar = chars[0]  // expected-error {{cannot assign value of type 'Int8' to type 'UInt8'}}
  }
}

// <rdar://problem/20789423> Unclear diagnostic for multi-statement closure with no return type
func r20789423() {
  class C {
    func f(_ value: Int) { }
  }
  
  let p: C
  print(p.f(p)())  // expected-error {{cannot convert value of type 'C' to expected argument type 'Int'}}
  
  let _f = { (v: Int) in  // expected-error {{unable to infer complex closure return type; add explicit type to disambiguate}} {{23-23=-> String }}
    print("a")
    return "hi"
  }
  
}

// Make sure that behavior related to allowing trailing closures to match functions
// with Any as a final parameter is the same after the changes made by SR-2505, namely:
// that we continue to select function that does _not_ have Any as a final parameter in
// presence of other possibilities.

protocol SR_2505_Initable { init() }
struct SR_2505_II : SR_2505_Initable {}

protocol P_SR_2505 {
  associatedtype T: SR_2505_Initable
}

extension P_SR_2505 {
  func test(it o: (T) -> Bool) -> Bool {
    return o(T.self())
  }
}

class C_SR_2505 : P_SR_2505 {
  typealias T = SR_2505_II

  func test(_ o: Any) -> Bool {
    return false
  }

  func call(_ c: C_SR_2505) -> Bool {
    // Note: no diagnostic about capturing 'self', because this is a
    // non-escaping closure -- that's how we know we have selected
    // test(it:) and not test(_)
    return c.test { o in test(o) }
  }
}

let _ = C_SR_2505().call(C_SR_2505())

// <rdar://problem/28909024> Returning incorrect result type from method invocation can result in nonsense diagnostic
extension Collection {
  func r28909024(_ predicate: (Iterator.Element)->Bool) -> Index {
    return startIndex
  }
}
func fn_r28909024(n: Int) {
  return (0..<10).r28909024 { // expected-error {{unexpected non-void return value in void function}}
    _ in true
  }
}

// SR-2994: Unexpected ambiguous expression in closure with generics
struct S_2994 {
  var dataOffset: Int
}
class C_2994<R> {
  init(arg: (R) -> Void) {}
}
func f_2994(arg: String) {}
func g_2994(arg: Int) -> Double {
  return 2
}
C_2994<S_2994>(arg: { (r: S_2994) in f_2994(arg: g_2994(arg: r.dataOffset)) }) // expected-error {{cannot convert value of type 'Double' to expected argument type 'String'}}

let _ = { $0[$1] }(1, 1) // expected-error {{cannot subscript a value of incorrect or ambiguous type}}
let _ = { $0 = ($0 = {}) } // expected-error {{assigning a variable to itself}}
let _ = { $0 = $0 = 42 } // expected-error {{assigning a variable to itself}}

// https://bugs.swift.org/browse/SR-403
// The () -> T => () -> () implicit conversion was kicking in anywhere
// inside a closure result, not just at the top-level.
let mismatchInClosureResultType : (String) -> ((Int) -> Void) = {
  (String) -> ((Int) -> Void) in
    return { }
    // expected-error@-1 {{contextual type for closure argument list expects 1 argument, which cannot be implicitly ignored}}
}

// SR-3520: Generic function taking closure with inout parameter can result in a variety of compiler errors or EXC_BAD_ACCESS
func sr3520_1<T>(_ g: (inout T) -> Int) {}
sr3520_1 { $0 = 1 } // expected-error {{cannot convert value of type '()' to closure result type 'Int'}}

func sr3520_2<T>(_ item: T, _ update: (inout T) -> Void) {
  var x = item
  update(&x)
}
var sr3250_arg = 42
sr3520_2(sr3250_arg) { $0 += 3 } // ok

// This test makes sure that having closure with inout argument doesn't crash with member lookup
struct S_3520 {
  var number1: Int
}
func sr3520_set_via_closure<S, T>(_ closure: (inout S, T) -> ()) {}
sr3520_set_via_closure({ $0.number1 = $1 }) // expected-error {{type of expression is ambiguous without more context}}

// SR-1976/SR-3073: Inference of inout
func sr1976<T>(_ closure: (inout T) -> Void) {}
sr1976({ $0 += 2 }) // ok

// SR-3073: UnresolvedDotExpr in single expression closure

struct SR3073Lense<Whole, Part> {
  let set: (inout Whole, Part) -> ()
}
struct SR3073 {
  var number1: Int
  func lenses() {
    let _: SR3073Lense<SR3073, Int> = SR3073Lense(
      set: { $0.number1 = $1 } // ok
    )
  }
}

// SR-3479: Segmentation fault and other error for closure with inout parameter
func sr3497_unfold<A, B>(_ a0: A, next: (inout A) -> B) {}
func sr3497() {
  let _ = sr3497_unfold((0, 0)) { s in 0 } // ok
}

// SR-3758: Swift 3.1 fails to compile 3.0 code involving closures and IUOs
let _: ((Any?) -> Void) = { (arg: Any!) in }
// This example was rejected in 3.0 as well, but accepting it is correct.
let _: ((Int?) -> Void) = { (arg: Int!) in }

// rdar://30429709 - We should not attempt an implicit conversion from
// () -> T to () -> Optional<()>.
func returnsArray() -> [Int] { return [] }

returnsArray().flatMap { $0 }.flatMap { }
// expected-warning@-1 {{expression of type 'Int' is unused}}
// expected-warning@-2 {{Please use map instead.}}
// expected-warning@-3 {{result of call to 'flatMap' is unused}}

// rdar://problem/30271695
_ = ["hi"].flatMap { $0.isEmpty ? nil : $0 }
