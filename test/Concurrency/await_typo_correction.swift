// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple -parse-as-library
// REQUIRES: concurrency

func asyncFunc() async throws {}

func anotherAsyncFunc() async -> Int {
  return 42
}

func async() throws { }

@main struct MyProgram {
  static func main() async throws {
    // expected-cparser-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{9-14=await}} expected-astgen-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{9-15=}}{{15-15=await }}
    try async asyncFunc()

    // expected-cparser-error@+2 {{found 'async' in expression; did you mean 'await'?}}{{5-10=await}} expected-astgen-error@+2 {{consecutive statements on a line must be separated by newline or ';'}}
    // expected-cparser-warning@+1 {{'try' must precede 'await'}}{{5-11=}}{{15-15=await }}
    async try asyncFunc() // expected-astgen-error {{function is unused}} expected-astgen-error {{expression is 'async' but is not marked with 'await'}} expected-astgen-note {{call is 'async'}}

    // expected-cparser-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{13-18=await}} expected-astgen-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{13-19=}}{{19-19=await }}
    let _ = async anotherAsyncFunc()

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
