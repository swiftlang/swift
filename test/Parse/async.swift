// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

// Parsing function declarations with 'async'
func asyncGlobal1() async { }
func asyncGlobal2() async throws { }

func asyncGlobal3() throws async { } // expected-error{{'async' must precede 'throws'}}{{28-34=}}{{21-21=async }}

func asyncGlobal3(fn: () throws -> Int) rethrows async { } // expected-error{{'async' must precede 'rethrows'}}{{50-56=}}{{41-41=async }}

func asyncGlobal4() -> Int async { } // expected-error{{'async' may only occur before '->'}}{{28-34=}}{{21-21=async }}

func asyncGlobal5() -> Int async throws { }
// expected-error@-1{{'async' may only occur before '->'}}{{28-34=}}{{21-21=async }}
// expected-error@-2{{'throws' may only occur before '->'}}{{34-41=}}{{21-21=throws }}

func asyncGlobal6() -> Int throws async { }
// expected-error@-1{{'throws' may only occur before '->'}}{{28-35=}}{{21-21=throws }}
// expected-error@-2{{'async' may only occur before '->'}}{{35-41=}}{{21-21=async }}

func asyncGlobal7() throws -> Int async { } // expected-error{{'async' may only occur before '->'}}{{35-41=}}{{21-21=async }}

func asyncGlobal8() async throws async -> async Int async {}
// expected-error@-1{{'async' has already been specified}} {{34-40=}}
// expected-error@-2{{'async' has already been specified}} {{43-49=}}
// expected-error@-3{{'async' has already been specified}} {{53-59=}}

class X {
  init() async { }

  deinit async { } // expected-error{{deinitializers cannot have a name}}

  func f() async { }

  subscript(x: Int) async -> Int { // expected-error{{expected '->' for subscript element type}}
    // expected-error@-1{{single argument function types require parentheses}}
    // expected-error@-2{{cannot find type 'async' in scope}}
    // expected-note@-3{{cannot use module 'async' as a type}}
    get {
      return 0
    }

    set async { // expected-error{{'set' accessor cannot have specifier 'async'}}
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
