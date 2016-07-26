// RUN: %target-parse-verify-swift

// Intentionally has lower precedence than assignments and ?:
infix operator %%%% : LowPrecedence
precedencegroup LowPrecedence {
  associativity: none
  lowerThan: AssignmentPrecedence
}
func %%%%<T,U>(x:T, y:U) -> Int { return 0 }

// Intentionally has lower precedence between assignments and ?:
infix operator %%% : MiddlingPrecedence
precedencegroup MiddlingPrecedence {
  associativity: none
  higherThan: AssignmentPrecedence
  lowerThan: TernaryPrecedence
}
func %%%<T,U>(x:T, y:U) -> Int { return 1 }

func foo() throws -> Int { return 0 }
func bar() throws -> Int { return 0 }

var x = try foo() + bar()
x = try foo() + bar()
x += try foo() + bar()
x += try foo() %%%% bar() // expected-error {{'try' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}} // expected-warning {{result of operator '%%%%' is unused}}
x += try foo() %%% bar()
x = foo() + try bar() // expected-error {{'try' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}

var y = true ? try foo() : try bar() + 0
var z = true ? try foo() : try bar() %%% 0 // expected-error {{'try' following conditional operator does not cover everything to its right}}

var a = try! foo() + bar()
a = try! foo() + bar()
a += try! foo() + bar()
a += try! foo() %%%% bar() // expected-error {{'try!' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}} // expected-warning {{result of operator '%%%%' is unused}}
a += try! foo() %%% bar()
a = foo() + try! bar() // expected-error {{'try!' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}

var b = true ? try! foo() : try! bar() + 0
var c = true ? try! foo() : try! bar() %%% 0 // expected-error {{'try!' following conditional operator does not cover everything to its right}}

infix operator ?+= : AssignmentPrecedence
func ?+=(lhs: inout Int?, rhs: Int?) {
  lhs = lhs! + rhs!
}

var i = try? foo() + bar()
let _: Double = i // expected-error {{cannot convert value of type 'Int?' to specified type 'Double'}}
i = try? foo() + bar()
i ?+= try? foo() + bar()
i ?+= try? foo() %%%% bar() // expected-error {{'try?' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}} // expected-warning {{result of operator '%%%%' is unused}}
i ?+= try? foo() %%% bar()
_ = foo() == try? bar() // expected-error {{'try?' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}
_ = (try? foo()) == bar() // expected-error {{call can throw but is not marked with 'try'}}
_ = foo() == (try? bar()) // expected-error {{call can throw but is not marked with 'try'}}
_ = (try? foo()) == (try? bar())

let j = true ? try? foo() : try? bar() + 0
let k = true ? try? foo() : try? bar() %%% 0 // expected-error {{'try?' following conditional operator does not cover everything to its right}}

try let singleLet = foo() // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{21-21=try }}
try var singleVar = foo() // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{21-21=try }}
try let uninit: Int // expected-error {{'try' must be placed on the initial value expression}}
try let (destructure1, destructure2) = (foo(), bar()) // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{40-40=try }}
try let multi1 = foo(), multi2 = bar() // expected-error {{'try' must be placed on the initial value expression}} expected-error 2 {{call can throw but is not marked with 'try'}}

func test() throws -> Int {
  try while true { // expected-error {{'try' cannot be used with 'while'}}
    try break // expected-error {{'try' cannot be used with 'break'}}
  }
  
  try throw // expected-error {{'try' must be placed on the thrown expression}} {{3-7=}} {{3-3=try }} expected-error {{expected expression in 'throw' statement}}
  ; // Reset parser.
  
  try return // expected-error {{'try' cannot be used with 'return'}} expected-error {{non-void function should return a value}}
  ; // Reset parser.

  try throw foo() // expected-error {{'try' must be placed on the thrown expression}} {{3-7=}} {{13-13=try }}
  // expected-error@-1 {{thrown expression type 'Int' does not conform to 'Error'}}
  try return foo() // expected-error {{'try' must be placed on the returned expression}} {{3-7=}} {{14-14=try }}
}

// Test operators.
func *(a : String, b : String) throws -> Int { return 42 }
let _ = "foo"
        *  // expected-error {{operator can throw but expression is not marked with 'try'}}
        "bar"
let _ = try! "foo"*"bar"
let _ = try? "foo"*"bar"
let _ = (try? "foo"*"bar") ?? 0


// <rdar://problem/21414023> Assertion failure when compiling function that takes throwing functions and rethrows
func rethrowsDispatchError(handleError: ((Error) throws -> ()), body: () throws -> ()) rethrows {
  do {
    body()   // expected-error {{call can throw but is not marked with 'try'}}
  } catch {
  }
}

// <rdar://problem/21432429> Calling rethrows from rethrows crashes Swift compiler
struct r21432429 {
  func x(_ f: () throws -> ()) rethrows {}
  func y(_ f: () throws -> ()) rethrows {
    x(f)  // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  }
}

// <rdar://problem/21427855> Swift 2: Omitting try from call to throwing closure in rethrowing function crashes compiler
func callThrowingClosureWithoutTry(closure: (Int) throws -> Int) rethrows {
  closure(0)  // expected-error {{call can throw but is not marked with 'try'}} expected-warning {{result of call is unused}}
}

func producesOptional() throws -> Int? { return nil }
let doubleOptional = try? producesOptional()
let _: String = doubleOptional // expected-error {{cannot convert value of type 'Int??' to specified type 'String'}}

func maybeThrow() throws {}
try maybeThrow() // okay
try! maybeThrow() // okay
try? maybeThrow() // okay since return type of maybeThrow is Void
_ = try? maybeThrow() // okay

let _: () -> Void = { try! maybeThrow() } // okay
let _: () -> Void = { try? maybeThrow() } // okay since return type of maybeThrow is Void


if try? maybeThrow() { // expected-error {{cannot be used as a boolean}} {{4-4=((}} {{21-21=) != nil)}}
}
let _: Int = try? foo() // expected-error {{value of optional type 'Int?' not unwrapped; did you mean to use 'try!' or chain with '?'?}} {{14-18=try!}}

class X {}
func test(_: X) {}
func producesObject() throws -> AnyObject { return X() }
test(try producesObject()) // expected-error {{'AnyObject' is not convertible to 'X'; did you mean to use 'as!' to force downcast?}} {{26-26= as! X}}

