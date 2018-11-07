// RUN: %target-typecheck-verify-swift -swift-version 5

// Intentionally has lower precedence than assignments and ?:
infix operator %%%% : LowPrecedence
precedencegroup LowPrecedence {
  associativity: none
  lowerThan: AssignmentPrecedence
}
func %%%%<T, U>(x: T, y: U) -> Int { return 0 }

// Intentionally has lower precedence between assignments and ?:
infix operator %%% : MiddlingPrecedence
precedencegroup MiddlingPrecedence {
  associativity: none
  higherThan: AssignmentPrecedence
  lowerThan: TernaryPrecedence
}
func %%%<T, U>(x: T, y: U) -> Int { return 1 }

func foo() throws -> Int { return 0 }
func bar() throws -> Int { return 0 }

var x = try foo() + bar()
x = try foo() + bar()
x += try foo() + bar()
x += try foo() %%%% bar() // expected-error {{'try' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}} // expected-warning {{result of operator '%%%%' is unused}}
                          // expected-note@-1 {{did you mean to use 'try'?}} {{21-21=try }}
                          // expected-note@-2 {{did you mean to handle error as optional value?}} {{21-21=try? }}
                          // expected-note@-3 {{did you mean to disable error propagation?}} {{21-21=try! }}
x += try foo() %%% bar()
x = foo() + try bar() // expected-error {{'try' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}
                      // expected-note@-1 {{did you mean to use 'try'?}} {{5-5=try }}
                      // expected-note@-2 {{did you mean to handle error as optional value?}} {{5-5=try? }}
                      // expected-note@-3 {{did you mean to disable error propagation?}} {{5-5=try! }}

var y = true ? try foo() : try bar() + 0
var z = true ? try foo() : try bar() %%% 0 // expected-error {{'try' following conditional operator does not cover everything to its right}}

var a = try! foo() + bar()
a = try! foo() + bar()
a += try! foo() + bar()
a += try! foo() %%%% bar() // expected-error {{'try!' following assignment operator does not cover everything to its right}} // expected-error {{call can throw but is not marked with 'try'}} // expected-warning {{result of operator '%%%%' is unused}}
                           // expected-note@-1 {{did you mean to use 'try'?}} {{22-22=try }}
                           // expected-note@-2 {{did you mean to handle error as optional value?}} {{22-22=try? }}
                           // expected-note@-3 {{did you mean to disable error propagation?}} {{22-22=try! }}
a += try! foo() %%% bar()
a = foo() + try! bar() // expected-error {{'try!' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}
                       // expected-note@-1 {{did you mean to use 'try'?}} {{5-5=try }}
                       // expected-note@-2 {{did you mean to handle error as optional value?}} {{5-5=try? }}
                       // expected-note@-3 {{did you mean to disable error propagation?}} {{5-5=try! }}

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
                            // expected-note@-1 {{did you mean to use 'try'?}} {{23-23=try }}
                            // expected-note@-2 {{did you mean to handle error as optional value?}} {{23-23=try? }}
                            // expected-note@-3 {{did you mean to disable error propagation?}} {{23-23=try! }}
i ?+= try? foo() %%% bar()
_ = foo() == try? bar() // expected-error {{'try?' cannot appear to the right of a non-assignment operator}} // expected-error {{call can throw but is not marked with 'try'}}
                        // expected-note@-1 {{did you mean to use 'try'?}} {{5-5=try }}
                        // expected-note@-2 {{did you mean to handle error as optional value?}} {{5-5=try? }}
                        // expected-note@-3 {{did you mean to disable error propagation?}} {{5-5=try! }}
_ = (try? foo()) == bar() // expected-error {{call can throw but is not marked with 'try'}}
                          // expected-note@-1 {{did you mean to use 'try'?}} {{21-21=try }}
                          // expected-note@-2 {{did you mean to handle error as optional value?}} {{21-21=try? }}
                          // expected-note@-3 {{did you mean to disable error propagation?}} {{21-21=try! }}
_ = foo() == (try? bar()) // expected-error {{call can throw but is not marked with 'try'}}
                          // expected-note@-1 {{did you mean to use 'try'?}} {{5-5=try }}
                          // expected-note@-2 {{did you mean to handle error as optional value?}} {{5-5=try? }}
                          // expected-note@-3 {{did you mean to disable error propagation?}} {{5-5=try! }}
_ = (try? foo()) == (try? bar())

let j = true ? try? foo() : try? bar() + 0
let k = true ? try? foo() : try? bar() %%% 0 // expected-error {{'try?' following conditional operator does not cover everything to its right}}

try let singleLet = foo() // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{21-21=try }}
try var singleVar = foo() // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{21-21=try }}
try let uninit: Int // expected-error {{'try' must be placed on the initial value expression}}
try let (destructure1, destructure2) = (foo(), bar()) // expected-error {{'try' must be placed on the initial value expression}} {{1-5=}} {{40-40=try }}
try let multi1 = foo(), multi2 = bar() // expected-error {{'try' must be placed on the initial value expression}} expected-error 2 {{call can throw but is not marked with 'try'}}
                                       // expected-note@-1 {{did you mean to use 'try'?}} {{18-18=try }} expected-note@-1 {{did you mean to use 'try'?}} {{34-34=try }}
                                       // expected-note@-2 {{did you mean to handle error as optional value?}} {{18-18=try? }} expected-note@-2 {{did you mean to handle error as optional value?}} {{34-34=try? }}
                                       // expected-note@-3 {{did you mean to disable error propagation?}} {{18-18=try! }} expected-note@-3 {{did you mean to disable error propagation?}} {{34-34=try! }}
class TryDecl { // expected-note {{in declaration of 'TryDecl'}}
  try let singleLet = foo() // expected-error {{'try' must be placed on the initial value expression}} {{3-7=}} {{23-23=try }}
                            // expected-error @-1 {{call can throw, but errors cannot be thrown out of a property initializer}}
  try var singleVar = foo() // expected-error {{'try' must be placed on the initial value expression}} {{3-7=}} {{23-23=try }}
                            // expected-error @-1 {{call can throw, but errors cannot be thrown out of a property initializer}}

  try // expected-error {{expected declaration}}
  func method() {}
}

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
           // expected-note@-1 {{did you mean to use 'try'?}} {{9-9=try }}
           // expected-note@-2 {{did you mean to handle error as optional value?}} {{9-9=try? }}
           // expected-note@-3 {{did you mean to disable error propagation?}} {{9-9=try! }}
        "bar"
let _ = try! "foo"*"bar"
let _ = try? "foo"*"bar"
let _ = (try? "foo"*"bar") ?? 0


// <rdar://problem/21414023> Assertion failure when compiling function that takes throwing functions and rethrows
func rethrowsDispatchError(handleError: ((Error) throws -> ()), body: () throws -> ()) rethrows {
  do {
    body()   // expected-error {{call can throw but is not marked with 'try'}}
             // expected-note@-1 {{did you mean to use 'try'?}} {{5-5=try }}
             // expected-note@-2 {{did you mean to handle error as optional value?}} {{5-5=try? }}
             // expected-note@-3 {{did you mean to disable error propagation?}} {{5-5=try! }}
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
  closure(0)  // expected-error {{call can throw but is not marked with 'try'}} expected-warning {{result of call to function returning 'Int' is unused}}
              // expected-note@-1 {{did you mean to use 'try'?}} {{3-3=try }}
              // expected-note@-2 {{did you mean to handle error as optional value?}} {{3-3=try? }}
              // expected-note@-3 {{did you mean to disable error propagation?}} {{3-3=try! }}
}

func producesOptional() throws -> Int? { return nil }
let singleOptional = try? producesOptional()
let _: String = singleOptional // expected-error {{cannot convert value of type 'Int?' to specified type 'String'}}

let _ = (try? foo())!! // expected-error {{cannot force unwrap value of non-optional type 'Int'}}

func producesDoubleOptional() throws -> Int?? { return 3 }
let _: String = try? producesDoubleOptional() // expected-error {{cannot convert value of type 'Int??' to specified type 'String'}}

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

_ = "a\(try maybeThrow())b"
_ = try "a\(maybeThrow())b"
_ = "a\(maybeThrow())" // expected-error {{call can throw but is not marked with 'try'}}
              // expected-note@-1 {{did you mean to use 'try'?}} {{9-9=try }}
              // expected-note@-2 {{did you mean to handle error as optional value?}} {{9-9=try? }}
              // expected-note@-3 {{did you mean to disable error propagation?}} {{9-9=try! }}

extension DefaultStringInterpolation {
  mutating func appendInterpolation() throws {}
}

_ = try "a\()b"
_ = "a\()b" // expected-error {{interpolation can throw but is not marked with 'try'}}
              // expected-note@-1 {{did you mean to use 'try'?}} {{5-5=try }}
              // expected-note@-2 {{did you mean to handle error as optional value?}} {{5-5=try? }}
              // expected-note@-3 {{did you mean to disable error propagation?}} {{5-5=try! }}
_ = try "\() \(1)"

func testGenericOptionalTry<T>(_ call: () throws -> T ) {
  let _: String = try? call() // expected-error {{cannot convert value of type 'T?' to specified type 'String'}}
}

func genericOptionalTry<T>(_ call: () throws -> T ) -> T? {
  let x = try? call() // no error expected
  return x
}

// Test with a non-optional type
let _: String = genericOptionalTry({ () throws -> Int in return 3 }) // expected-error {{cannot convert value of type 'Int?' to specified type 'String'}}

// Test with an optional type
let _: String = genericOptionalTry({ () throws -> Int? in return nil }) // expected-error {{cannot convert value of type 'Int??' to specified type 'String'}}

func produceAny() throws -> Any {
  return 3
}

let _: Int? = try? produceAny() as? Int // good
let _: Int? = (try? produceAny()) as? Int // good
let _: String = try? produceAny() as? Int // expected-error {{cannot convert value of type 'Int?' to specified type 'String'}}
let _: String = (try? produceAny()) as? Int // expected-error {{cannot convert value of type 'Int?' to specified type 'String'}}


struct ThingProducer {
  func produceInt() throws -> Int { return 3 }
  func produceIntNoThrowing() -> Int { return 3 }
  func produceAny() throws -> Any { return 3 }
  func produceOptionalAny() throws -> Any? { return 3 }
  func produceDoubleOptionalInt() throws -> Int?? { return 3 }
}

let optProducer: ThingProducer? = ThingProducer()
let _: Int? = try? optProducer?.produceInt()
let _: Int = try? optProducer?.produceInt() // expected-error {{cannot convert value of type 'Int?' to specified type 'Int'}}
let _: String = try? optProducer?.produceInt() // expected-error {{cannot convert value of type 'Int?' to specified type 'String'}}
let _: Int?? = try? optProducer?.produceInt() // This was the expected type before Swift 5, but this still works; just adds more optional-ness

let _: Int? = try? optProducer?.produceIntNoThrowing() // expected-warning {{no calls to throwing functions occur within 'try' expression}}

let _: Int? = (try? optProducer?.produceAny()) as? Int // good
let _: Int? = try? optProducer?.produceAny() as? Int // good
let _: Int?? = try? optProducer?.produceAny() as? Int // good
let _: String = try? optProducer?.produceAny() as? Int // expected-error {{cannot convert value of type 'Int?' to specified type 'String'}}

let _: String = try? optProducer?.produceDoubleOptionalInt() // expected-error {{cannot convert value of type 'Int??' to specified type 'String'}}

let producer = ThingProducer()

let _: Int = try? producer.produceDoubleOptionalInt() // expected-error {{cannot convert value of type 'Int??' to specified type 'Int'}}

// We don't offer 'try!' here because it would not change the type of the expression in Swift 5+
let _: Int? = try? producer.produceDoubleOptionalInt() // expected-error {{cannot convert value of type 'Int??' to specified type 'Int?'}}

let _: Int?? = try? producer.produceDoubleOptionalInt() // good
let _: Int??? = try? producer.produceDoubleOptionalInt() // good
let _: String = try? producer.produceDoubleOptionalInt() // expected-error {{cannot convert value of type 'Int??' to specified type 'String'}}
