// RUN: %target-parse-verify-swift

func myMap<T1, T2>(array: [T1], _ fn: (T1) -> T2) -> [T2] {}

var intArray : [Int]

myMap(intArray, { String($0) })
myMap(intArray, { x -> String in String(x) } )

// Closures with too few parameters.
func foo(x: (Int, Int) -> Int) {}
foo({$0}) // expected-error{{cannot invoke 'foo' with an argument list of type '((_) -> _)'}} expected-note{{expected an argument list of type '((Int, Int) -> Int)'}}

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

f0(X2(), {$0.g()})  // expected-error {{cannot invoke 'f0' with an argument list of type '(X2, (_) -> _)'}} expected-note{{expected an argument list of type '(T, (inout T) -> U)'}}

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

