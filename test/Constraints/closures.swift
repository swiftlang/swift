// RUN: %target-parse-verify-swift

func myMap<T1, T2>(array: [T1], _ fn: (T1) -> T2) -> [T2] {}

var intArray : [Int]

myMap(intArray, { String($0) })
myMap(intArray, { x -> String in String(x) } )

// Closures with too few parameters.
func foo(x: (Int, Int) -> Int) {}
foo({$0}) // expected-error{{cannot convert value of type '(Int, Int)' to closure result type 'Int'}}

struct X {}
func mySort(array: [String], _ predicate: (String, String) -> Bool) -> [String] {}
func mySort(array: [X], _ predicate: (X, X) -> Bool) -> [X] {}
var strings : [String]
mySort(strings, { x, y in x < y })

// Closures with inout arguments.
func f0<T, U>(t: T, _ f: (inout T) -> U) -> U {
  var t2 = t;
  return f(&t2)
}

struct X2 {
  func g() -> Float { return 0 }  
}

f0(X2(), {$0.g()})  // expected-error {{type of expression is ambiguous without more context}}

// Autoclosure
func f1(@autoclosure f f: () -> Int) { }
func f2() -> Int { }
f1(f: f2) // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}{{9-9=()}}
f1(f: 5)

// Ternary in closure
var evenOrOdd : Int -> String = {$0 % 2 == 0 ? "even" : "odd"}

// <rdar://problem/15367882>
func foo() {
  not_declared({ $0 + 1 }) // expected-error{{use of unresolved identifier 'not_declared'}}
}

// <rdar://problem/15536725>
struct X3<T> {
  init(_: (T)->()) {}
}

func testX3(var x: Int) {
  _ = X3({ x = $0 })
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
      print("Hello")
  }
  inSubcall = false

  // This is a problem, but isn't clear what was intended.
  var somethingElse = true { // expected-error {{cannot call value of non-function type 'Bool'}}
      print("Hello")
  }
  inSubcall = false

}

// <rdar://problem/22162441> Crash from failing to diagnose nonexistent method access inside closure
func r22162441(lines: [String]) {
  _ = lines.map { line in line.fooBar() }  // expected-error {{value of type 'String' has no member 'fooBar'}}
  _ = lines.map { $0.fooBar() }  // expected-error {{value of type 'String' has no member 'fooBar'}}
}


func testMap() {
  let a = 42
  [1,a].map { $0 + 1.0 } // expected-error {{binary operator '+' cannot be applied to operands of type 'Int' and 'Double'}}
  // expected-note @-1 {{overloads for '+' exist with these partially matching parameter lists: (Int, Int), (Double, Double), (Int, UnsafeMutablePointer<Memory>), (Int, UnsafePointer<Memory>)}}
}

// <rdar://problem/22414757> "UnresolvedDot" "in wrong phase" assertion from verifier
[].reduce { $0 + $1 }  // expected-error {{cannot convert value of type '(_, _) -> _' to expected argument type '(_, combine: @noescape (_, _) throws -> _)'}}




// <rdar://problem/22333281> QoI: improve diagnostic when contextual type of closure disagrees with arguments
var _: ()-> Int = {0}

// expected-error @+1 {{contextual type for closure argument list expects 1 argument, but 0 were specified}}
var _: (Int)-> Int = {0}

// expected-error @+1 {{tuple types '(Int, Int)' and '()' have a different number of elements (2 vs. 0)}}
var _: (Int, Int)-> Int = {0}

// expected-error @+1 {{tuple types '(Int, Int)' and '(_, _, _)' have a different number of elements (2 vs. 3)}}
var _: (Int,Int)-> Int = {$0+$1+$2}

// expected-error @+1 {{tuple types '(Int, Int, Int)' and '(_, _)' have a different number of elements (3 vs. 2)}}
var _: (Int, Int, Int)-> Int = {$0+$1}


var _: ()-> Int = {a in 0}

// expected-error @+1 {{contextual type for closure argument list expects 1 argument, but 2 were specified}}
var _: (Int)-> Int = {a,b in 0}

// expected-error @+1 {{contextual type for closure argument list expects 1 argument, but 3 were specified}}
var _: (Int)-> Int = {a,b,c in 0}

var _: (Int, Int)-> Int = {a in 0}

// expected-error @+1 {{tuple types '(Int, Int, Int)' and '(_, _)' have a different number of elements (3 vs. 2)}}
var _: (Int, Int, Int)-> Int = {a, b in a+b}



