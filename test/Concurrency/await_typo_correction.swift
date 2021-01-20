// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -parse-as-library
// REQUIRES: concurrency

func asyncFunc() async throws {}

func anotherAsyncFunc() async -> Int {
  return 42
}

@main struct MyProgram {
  static func main() async throws {
    // expected-error@+7 {{cannot find 'async' in scope; did you mean 'await'?}}{{9-14=await}}
    // expected-error@+6 {{consecutive statements on a line must be separated by ';'}}
    // expected-error@+5 {{call is 'async' but is not marked with 'await'}}
    // expected-error@+4 {{call can throw but is not marked with 'try'}}
    // expected-note@+3 {{did you mean to use 'try'?}}
    // expected-note@+2 {{did you mean to handle error as optional value?}}
    // expected-note@+1 {{did you mean to disable error propagation?}}
    try async asyncFunc()

    // expected-error@+4 {{consecutive statements on a line must be separated by ';'}}
    // expected-error@+3 {{cannot find 'async' in scope; did you mean 'await'?}}{{13-18=await}}
    // expected-warning@+2 {{result of call to 'anotherAsyncFunc()' is unused}}
    // expected-error@+1 {{call is 'async' but is not marked with 'await'}}
    let _ = async anotherAsyncFunc()

    // Don't emit a diagnostic here
    async let foo = anotherAsyncFunc()
    let _ = await foo
  }
}
