// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -parse-as-library
// REQUIRES: concurrency

func asyncFunc() async throws {}

func anotherAsyncFunc() async -> Int {
  return 42
}

func async() throws { }

@main struct MyProgram {
  static func main() async throws {
    // expected-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{9-14=await}}
    try async asyncFunc()

    // expected-error@+1 {{found 'async' in expression; did you mean 'await'?}}{{13-18=await}}
    let _ = async anotherAsyncFunc()

    // Don't emit a diagnostic here
    async let foo = anotherAsyncFunc()
    let _ = await foo

    // I question the name choice, but it's valid
    try async()
  }
}
