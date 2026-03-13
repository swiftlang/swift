// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=targeted
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

// These tests cover various interactions with async functions that are
// either throws or rethrows.
// See rdar://70813762 and rdar://70751405

enum InvocationError : Error {
  case ErrVal
}

func asyncThrows() async throws {
  throw InvocationError.ErrVal
}

// T = Int
func asyncRethrows(fn : () async throws -> Int) async rethrows -> Int {
  return try await fn()
}

// T = String
func asyncRethrows(fn : () async throws -> String) async rethrows -> String {
  return try await fn()
}

// Generic. NOTE the 'rethrows'
func invoke<T>(fn : () async throws -> T) async rethrows -> T {
  return try await fn()
}

// NOTE the 'rethrows'
func invokeAuto<T>(_ val : @autoclosure () async throws -> T) async rethrows -> T {
  return try await val()
}

func normalTask() async -> Int {
  return 42
}

func throwingTask() async throws -> String {
  if 1.0 / 3.0 == 0.33 {
    throw InvocationError.ErrVal
  }
  return "ok!"
}

// expected-note@+1 7 {{add 'async' to function 'syncTest()' to make it asynchronous}} {{16-16= async}}
func syncTest() {
  let _ = invoke(fn: normalTask) // expected-error{{'async' call in a function that does not support concurrency}}
  let _ = invokeAuto(42) // expected-error{{'async' call in a function that does not support concurrency}}
  let _ = invokeAuto("intuitive") // expected-error{{'async' call in a function that does not support concurrency}}

  let _ = try! asyncRethrows(fn: throwingTask) // expected-error{{'async' call in a function that does not support concurrency}}
  let _ = try? invoke(fn: throwingTask) // expected-error{{'async' call in a function that does not support concurrency}}
  do {
   let _ = try invoke(fn: throwingTask) // expected-error{{'async' call in a function that does not support concurrency}}
   let _ = try asyncThrows() // expected-error{{'async' call in a function that does not support concurrency}}
  } catch {
    // ignore it
  }
}


func asyncTest() async {
  ///////////
  // tests that also omit await
  // expected-error@+2{{expression is 'async' but is not marked with 'await'}}{{11-11=await }}
  // expected-note@+1{{call is 'async'}}
  let _ = invoke(fn: normalTask)
  // expected-error@+2{{expression is 'async' but is not marked with 'await'}}{{11-11=await }}
  // expected-note@+1{{call is 'async'}}
  let _ = asyncRethrows(fn: normalTask)
  // expected-error@+2{{expression is 'async' but is not marked with 'await'}}{{11-11=await }}
  // expected-note@+1{{call is 'async'}}
  let _ = invokeAuto(42)

  // expected-error@+3 {{call can throw, but it is not marked with 'try' and the error is not handled}}
  // expected-error@+2 {{expression is 'async' but is not marked with 'await'}}{{11-11=await }}
  // expected-note@+1 {{call is 'async'}}
  let _ = asyncThrows()

  // expected-note@+4{{call is to 'rethrows' function, but argument function can throw}}
  // expected-error@+3{{call can throw, but it is not marked with 'try' and the error is not handled}}
  // expected-error@+2{{expression is 'async' but is not marked with 'await'}}{{11-11=await }}
  // expected-note@+1:11{{call is 'async'}}
  let _ = invoke(fn: throwingTask)

  ///////////
  // tests that use await and handles the exceptions

  // expected-note@+2{{call is to 'rethrows' function, but argument function can throw}}
  // expected-error@+1{{call can throw, but it is not marked with 'try' and the error is not handled}}
  let _ = await invoke(fn: throwingTask)
  let _ = await invoke(fn: normalTask) // ok

  let _ = await asyncRethrows(fn: normalTask)
  let _ = try! await asyncRethrows(fn: normalTask) // expected-warning{{no calls to throwing functions occur within 'try' expression}}
  let _ = try? await asyncRethrows(fn: normalTask) // expected-warning{{no calls to throwing functions occur within 'try' expression}}

  let _ = try! await asyncRethrows(fn: throwingTask)
  let _ = try? await asyncRethrows(fn: throwingTask)
  let _ = try! await asyncThrows()
  let _ = try? await asyncThrows()

  //////////
  // some auto-closure tests

  let _ = await invokeAuto("intuitive")
  let _ = try! await invokeAuto(await throwingTask())
  let _ = try? await invokeAuto(await throwingTask())
  let _ = await invokeAuto(try! await throwingTask())
  let _ = await invokeAuto(try? await throwingTask())
  // expected-error@+1{{expression is 'async' but is not marked with 'await'}}{{33-33=await }}
  let _ = await invokeAuto(try! throwingTask()) // expected-note@:33{{call is 'async' in an autoclosure argument}}
  // expected-error@+1{{expression is 'async' but is not marked with 'await'}}{{33-33=await }}
  let _ = await invokeAuto(try? throwingTask()) // expected-note@:33{{call is 'async' in an autoclosure argument}}

  let _ = await invokeAuto(try! await throwingTask())
  let _ = await invokeAuto(try? await throwingTask())
  /////////

  do {
    let _ = try await asyncThrows()
    let _ = try await asyncRethrows(fn: throwingTask)

    //////
    // more auto-closure tests

    // expected-note@+7 {{did you mean to disable error propagation?}}
    // expected-note@+6 {{did you mean to handle error as optional value?}}
    // expected-note@+5 {{did you mean to use 'try'?}}
    // expected-note@+4 {{call is to 'rethrows' function, but argument function can throw}}
    // expected-note@+3 {{call is 'async' in an autoclosure argument}}
    // expected-error@+2 {{expression is 'async' but is not marked with 'await'}}{{30-30=await }}
    // expected-error@+1 2 {{call can throw but is not marked with 'try'}}
    let _ = await invokeAuto(throwingTask())

    // expected-error@+1:34{{expression is 'async' but is not marked with 'await'}}{{34-34=await }}
    let _ = try await invokeAuto("hello" + throwingTask()) // expected-note@:44{{call is 'async' in an autoclosure argument}}

    // expected-error@+1{{expression is 'async' but is not marked with 'await'}}{{17-17=await }}
    let _ = try invokeAuto(await throwingTask())  // expected-note{{call is 'async'}}
    let _ = try await invokeAuto(await throwingTask())
  } catch {
    // ignore
  }
}

// rdar://123356909 - spurious error - `call can throw, but it is not marked with 'try' and the error is not handled`
do {
  struct AsyncTestSequence<Element>: AsyncSequence {
    typealias Element = Element

    let stream: AsyncMapSequence<AsyncStream<[String : Any]>, Element>

    init(stream: AsyncMapSequence<AsyncStream<[String : Any]>, Element>) {
      self.stream = stream
    }

    func makeAsyncIterator() -> AsyncIterator {
      .init(iterator: stream.makeAsyncIterator())
    }

    struct AsyncIterator: AsyncIteratorProtocol {
      var iterator: AsyncMapSequence<AsyncStream<[String : Any]>, Element>.AsyncIterator

      mutating func next() async -> Element? {
        await iterator.next() // Ok
      }
    }
  }
}
