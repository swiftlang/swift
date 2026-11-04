// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple -parse-as-library
// REQUIRES: concurrency

func asyncFunc() async throws {}

func anotherAsyncFunc() async -> Int {
  return 42
}

func async() throws { }

@main struct MyProgram {
  static func main() async throws {
    // expected-cparser-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{9-14=await}} expected-astgen-error@+1 {{consecutive statements on a line must be separated by newline or ';'}}
    try async asyncFunc() // expected-astgen-warning {{no calls to throwing functions occur within 'try' expression}} expected-astgen-error {{function is unused}} expected-astgen-error {{call can throw but is not marked with 'try'}} expected-astgen-note {{did you mean to use 'try'?}} expected-astgen-note {{did you mean to handle error as optional value?}} expected-astgen-note {{did you mean to disable error propagation?}} expected-astgen-error {{expression is 'async' but is not marked with 'await'}} expected-astgen-note {{call is 'async'}}

    // expected-cparser-error@+2 {{found 'async' in expression; did you mean 'await'?}}{{5-10=await}} expected-astgen-error@+2 {{consecutive statements on a line must be separated by newline or ';'}}
    // expected-cparser-warning@+1 {{'try' must precede 'await'}}{{5-11=}}{{15-15=await }}
    async try asyncFunc() // expected-astgen-error {{function is unused}} expected-astgen-error {{expression is 'async' but is not marked with 'await'}} expected-astgen-note {{call is 'async'}}

    // expected-cparser-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{13-18=await}} expected-astgen-error@+1 {{consecutive statements on a line must be separated by newline or ';'}}
    let _ = async anotherAsyncFunc() // expected-astgen-warning {{result of call to 'anotherAsyncFunc()' is unused}} expected-astgen-error {{expression is 'async' but is not marked with 'await'}} expected-astgen-note {{call is 'async'}}

    // Don't emit a diagnostic here
    async let foo = anotherAsyncFunc()
    let _ = await foo

    // I question the name choice, but it's valid
    try async()
  }
}

func varNamedAsync(async: Bool) async {
  if async { }
  let _ = async
}
