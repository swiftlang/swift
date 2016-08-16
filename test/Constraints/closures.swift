// RUN: %target-parse-verify-swift

func myMap<T1, T2>(_ array: [T1], _ fn: (T1) -> T2) -> [T2] {}

var intArray : [Int]

_ = myMap(intArray, { String($0) })
_ = myMap(intArray, { x -> String in String(x) } )

// Closures with too few parameters.
func foo(_ x: ((Int, Int)) -> Int) {}
foo({$0}) // expected-error{{cannot convert value of type '(Int, Int)' to closure result type 'Int'}}

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
func r21544303() {
  var inSubcall = true
  {   // expected-error {{expected 'do' keyword to designate a block of statements}} {{3-3=do }}
  }
  inSubcall = false

  // This is a problem, but isn't clear what was intended.
  var somethingElse = true { // expected-error {{cannot call value of non-function type 'Bool'}}
  }
  inSubcall = false

  var v2 : Bool = false
  v2 = true
  {  // expected-error {{expected 'do' keyword to designate a block of statements}}
  }
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

var _: ((Int, Int)) -> Int = {a in 0}

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
func someFunc(_ foo: (@escaping (String) -> String)?, 
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
  // expected-note@-1{{parameter 'ac1' is implicitly non-escaping because it was declared @autoclosure}}
  acceptNothingToInt({ac1($0)})
  // expected-error@-1{{contextual closure type '() -> Int' expects 0 arguments, but 1 was used in closure body}}
  // FIXME: expected-error@-2{{closure use of non-escaping parameter 'ac1' may allow it to escape}}
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
var afterMessageCount : Int? = nil

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

let f: (Int, Int) -> Void = { x in }  // expected-error {{contextual closure type specifies '(Int, Int)', but 1 was used in closure body, try adding extra parentheses around the single tuple argument}}
