// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

// expected-note@+1{{add 'async' to function 'missingAsync' to make it asynchronous}}
func missingAsync<T : AsyncSequence>(_ seq: T) throws { 
  for try await _ in seq { } // expected-error{{'async' in a function that does not support concurrency}}
}

func missingThrows<T : AsyncSequence>(_ seq: T) async {
  for try await _ in seq { } // expected-error{{error is not handled because the enclosing function is not declared 'throws'}}
}

func executeAsync(_ work: () async -> Void) { }
func execute(_ work: () -> Void) { }

func missingThrowingInBlock<T : AsyncSequence>(_ seq: T) { 
  executeAsync { // expected-error{{invalid conversion from throwing function of type '() async throws -> Void' to non-throwing function type '() async -> Void'}}
    for try await _ in seq { }
  }
}

func missingTryInBlock<T : AsyncSequence>(_ seq: T) { 
  executeAsync { 
    for await _ in seq { } // expected-error{{call can throw, but the error is not handled}}
  }
}

func missingAsyncInBlock<T : AsyncSequence>(_ seq: T) { 
  execute { // expected-error{{invalid conversion from 'async' function of type '() async -> Void' to synchronous function type '() -> Void'}}
    do { 
      for try await _ in seq { }
    } catch { }
  }
}

func doubleDiagCheckGeneric<T : AsyncSequence>(_ seq: T) async {
  var it = seq.makeAsyncIterator()
  // expected-note@+2{{call is to 'rethrows' function, but a conformance has a throwing witness}}
  // expected-error@+1{{call can throw, but it is not marked with 'try' and the error is not handled}}
  let _ = await it.next()
}

struct ThrowingAsyncSequence: AsyncSequence, AsyncIteratorProtocol {
  typealias Element = Int
  typealias AsyncIterator = Self
  mutating func next() async throws -> Int? {
    return nil
  }

  func makeAsyncIterator() -> Self { return self }
}

func doubleDiagCheckConcrete(_ seq: ThrowingAsyncSequence) async {
  var it = seq.makeAsyncIterator()
  // expected-error@+1{{call can throw, but it is not marked with 'try' and the error is not handled}}
  let _ = await it.next()
}

// rdar://75274975
func forAwaitInsideDoCatch<Source: AsyncSequence>(_ source: Source) async {
  do {
    for try await item in source {
      print(item)
    }
  } catch {} // no-warning
}
