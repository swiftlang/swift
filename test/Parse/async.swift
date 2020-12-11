// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency

// REQUIRES: concurrency

// Parsing function declarations with 'async'
func asyncGlobal1() async { }
func asyncGlobal2() async throws { }

func asyncGlobal3() throws async { } // expected-error{{'async' must precede 'throws'}}{{28-34=}}{{21-21=async }}

func asyncGlobal3(fn: () throws -> Int) rethrows async { } // expected-error{{'async' must precede 'rethrows'}}{{50-56=}}{{41-41=async }}

func asyncGlobal4() -> Int async { } // expected-error{{'async' may only occur before '->'}}{{28-34=}}{{21-21=async }}

// If both async and throws follow the return type in the correct order,
// throws will get the error and fix-it first. Applying the fix-it will cause
// the async error and fix-it to be emitted. Applying that will place the async
// in the appropriate place.

// expected-error@+1{{'throws' may only occur before '->'}}{{34-41=}}{{21-21=throws }}
func asyncGlobal5() -> Int async throws { }

// If they are in the wrong order, the first fix-it will put them in the right
// order and will be the same as the above case
// expected-error@+1{{'async' must precede 'throws'}}{{35-41=}}{{28-28=async }}
func asyncGlobal6() -> Int throws async { }

func asyncGlobal7() throws -> Int async { } // expected-error{{'async' may only occur before '->'}}{{35-41=}}{{28-28=async }}

class X {
  init() async { } // expected-error{{initializer cannot be marked 'async'}}

  deinit async { } // expected-error{{deinitializers cannot have a name}}

  func f() async { }

  subscript(x: Int) async -> Int { // expected-error{{expected '->' for subscript element type}}
    // expected-error@-1{{single argument function types require parentheses}}
    // expected-error@-2{{cannot find type 'async' in scope}}
    // expected-note@-3{{cannot use module 'async' as a type}}
    get {
      return 0
    }

    set async { // expected-error{{expected '{' to start setter definition}}
    }
  }
}

// Parsing function types with 'async'.
typealias AsyncFunc1 = () async -> ()
typealias AsyncFunc2 = () async throws -> ()
typealias AsyncFunc3 = () throws async -> () // expected-error{{'async' must precede 'throws'}}{{34-40=}}{{27-27=async }}

// Parsing type expressions with 'async'.
func testTypeExprs() {
  let _ = [() async -> ()]()
  let _ = [() async throws -> ()]()
  let _ = [() throws async -> ()]()  // expected-error{{'async' must precede 'throws'}}{{22-28=}}{{15-15=async }}

  let _ = [() -> async ()]() // expected-error{{'async' may only occur before '->'}}{{18-24=}}{{15-15=async }}
}

// Parsing await syntax.
struct MyFuture {
  func await() -> Int { 0 }
}

func testAwaitExpr() async {
  let _ = await asyncGlobal1()
  let myFuture = MyFuture()
  let _ = myFuture.await()
}

func getIntSomeday() async -> Int { 5 }

func testAsyncLet() async {
  async let x = await getIntSomeday()
  _ = await x
}

async func asyncIncorrectly() { } // expected-error{{'async' must be written after the parameter list of a function}}{{1-7=}}{{30-30= async}}
