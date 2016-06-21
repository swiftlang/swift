// RUN: %target-parse-verify-swift

func myMap<T1, T2>(_ array: [T1], _ fn: (T1) -> T2) -> [T2] {}

var intArray : [Int]

_ = myMap(intArray, { String($0) })
_ = myMap(intArray, { x -> String in String(x) } )

// Closures with too few parameters.
func foo(_ x: (Int, Int) -> Int) {}
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
// expected-note @+1 {{in call to function 'callCC'}}
func callCC<U>(_ f: (CC) -> U) -> () {}

func typeCheckMultiStmtClosureCrash() {
  callCC { // expected-error {{generic parameter 'U' could not be inferred}}
    _ = $0
    return 1
  }
}

// SR-832 - both these should be ok
func someFunc(_ foo: ((String) -> String)?, bar: (String) -> String) {
    let _: (String) -> String = foo != nil ? foo! : bar
    let _: (String) -> String = foo ?? bar
}


// SR-1069 - Error diagnostic refers to wrong argument
class SR1069_W<T> {
  func append<Key: AnyObject where Key: Hashable>(value: T, forKey key: Key) {}
}
class SR1069_C<T> { let w: SR1069_W<(AnyObject, T) -> ()> = SR1069_W() }
struct S<T> {
  let cs: [SR1069_C<T>] = []

  func subscribe<Object: AnyObject where Object: Hashable>(object: Object?, method: (Object, T) -> ()) {
    let wrappedMethod = { (object: AnyObject, value: T) in }
    // expected-error @+1 {{cannot convert value of type '(AnyObject, T) -> ()' to expected argument type '(AnyObject, _) -> ()'}}
    cs.forEach { $0.w.append(value: wrappedMethod, forKey: object) }
  }
}

