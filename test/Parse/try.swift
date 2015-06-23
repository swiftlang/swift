// RUN: %target-parse-verify-swift

// Intentionally has lower precedence than assignments and ?:
infix operator %%%% { associativity none precedence 50 }
func %%%%<T,U>(x:T, y:U) -> Int { return 0 }

// Intentionally has lower precedence between assignments and ?:
infix operator %%% { associativity none precedence 95 }
func %%%<T,U>(x:T, y:U) -> Int { return 1 }

func foo() throws -> Int { return 0 }
func bar() throws -> Int { return 0 }

var x = try foo() + bar()
x = try foo() + bar()
x += try foo() + bar()
x += try foo() %%%% bar() // expected-error {{'try' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}}
x += try foo() %%% bar()
x = foo() + try bar() // expected-error {{'try' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}

var y = true ? try foo() : try bar() + 0
var z = true ? try foo() : try bar() %%% 0 // expected-error {{'try' following conditional operator does not cover everything to its right}}

var a = try! foo() + bar()
a = try! foo() + bar()
a += try! foo() + bar()
a += try! foo() %%%% bar() // expected-error {{'try!' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}}
a += try! foo() %%% bar()
a = foo() + try! bar() // expected-error {{'try!' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}

var b = true ? try! foo() : try! bar() + 0
var c = true ? try! foo() : try! bar() %%% 0 // expected-error {{'try!' following conditional operator does not cover everything to its right}}

try let singleLet = foo() // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{21-21=try }}
try var singleVar = foo() // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{21-21=try }}
try let uninit: Int // expected-error {{'try' must be placed on the initial value expression}}
try let (destructure1, destructure2) = (foo(), bar()) // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{40-40=try }}
try let multi1 = foo(), multi2 = bar() // expected-error {{'try' must be placed on the initial value expression}} expected-error 2 {{call can throw but is not marked with 'try'}}

func test() throws -> Int {
  try while true { // expected-error {{'try' cannot be used with 'while'}}
    try break // expected-error {{'try' cannot be used with 'break'}}
  }
  
  try throw // expected-error {{'try' must be placed on the thrown expression}} expected-error {{expected expression in 'throw' statement}}
  ; // Reset parser.
  
  try return // expected-error {{'try' cannot be used with 'return'}} expected-error {{non-void function should return a value}}
  ; // Reset parser.

  try throw foo() // expected-error {{'try' must be placed on the thrown expression}} {{3-7=}} {{13-13=try }}
  // expected-error@-1 {{does not conform to protocol 'ErrorType'}}
  try return foo() // expected-error {{'try' must be placed on the returned expression}} {{3-7=}} {{14-14=try }}
}

// Test operators.
func *(a : String, b : String) throws -> Int { return 42 }
let _ = "foo"
        *  // expected-error {{operator can throw but expression is not marked with 'try'}}
        "bar"
let _ = try! "foo"*"bar"


// <rdar://problem/21414023> Assertion failure when compiling function that takes throwing functions and rethrows
func rethrowsDispatchError(handleError: ((ErrorType) throws -> ()), body: () throws -> ()) rethrows {
  do {
    body()   // expected-error {{call can throw but is not marked with 'try'}}
  } catch {
  }
}

// <rdar://problem/21432429> Calling rethrows from rethrows crashes Swift compiler
struct r21432429 {
  func x(f: () throws ->()) rethrows {}
  func y(f: () throws ->()) rethrows {
    x(f)  // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  }
}

// <rdar://problem/21427855> Swift 2: Omitting try from call to throwing closure in rethrowing function crashes compiler
func callThrowingClosureWithoutTry(closure: Int throws -> Int) rethrows {
  closure(0)  // expected-error {{call can throw but is not marked with 'try'}}
}

