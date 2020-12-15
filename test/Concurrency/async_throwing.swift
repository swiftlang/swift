// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
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
  return await try fn()
}

// T = String
func asyncRethrows(fn : () async throws -> String) async rethrows -> String {
  return await try fn()
}

// Generic. NOTE the 'rethrows'
func invoke<T>(fn : () async throws -> T) async rethrows -> T {
  return await try fn()
}

// NOTE the 'rethrows'
func invokeAuto<T>(_ val : @autoclosure () async throws -> T) async rethrows -> T {
  return await try val()
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

// expected-note@+2 7 {{add '@asyncHandler' to function 'syncTest()' to create an implicit asynchronous context}} {{1-1=@asyncHandler }}
// expected-note@+1 7 {{add 'async' to function 'syncTest()' to make it asynchronous}} {{none}}
func syncTest() {
  let _ = invoke(fn: normalTask) // expected-error{{'async' in a function that does not support concurrency}}
  let _ = invokeAuto(42) // expected-error{{'async' in a function that does not support concurrency}}
  let _ = invokeAuto("intuitive") // expected-error{{'async' in a function that does not support concurrency}}
  
  let _ = try! asyncRethrows(fn: throwingTask) // expected-error{{'async' in a function that does not support concurrency}}
  let _ = try? invoke(fn: throwingTask) // expected-error{{'async' in a function that does not support concurrency}}
  do {
   let _ = try invoke(fn: throwingTask) // expected-error{{'async' in a function that does not support concurrency}}
   let _ = try asyncThrows() // expected-error{{'async' in a function that does not support concurrency}}
  } catch {
    // ignore it
  }
}


func asyncTest() async {
  ///////////
  // tests that also omit await

  let _ = invoke(fn: normalTask) // expected-error{{call is 'async' but is not marked with 'await'}}
  let _ = asyncRethrows(fn: normalTask) // expected-error{{call is 'async' but is not marked with 'await'}}
  let _ = invokeAuto(42) // expected-error{{call is 'async' but is not marked with 'await'}}

  // expected-error@+2 {{call can throw, but it is not marked with 'try' and the error is not handled}}
  // expected-error@+1 {{call is 'async' but is not marked with 'await'}}
  let _ = asyncThrows()
  
  // expected-note@+3{{call is to 'rethrows' function, but argument function can throw}}
  // expected-error@+2{{call can throw, but it is not marked with 'try' and the error is not handled}}
  // expected-error@+1{{call is 'async' but is not marked with 'await'}}
  let _ = invoke(fn: throwingTask)

  ///////////
  // tests that use await and handles the exceptions

  // expected-note@+2{{call is to 'rethrows' function, but argument function can throw}}
  // expected-error@+1{{call can throw, but it is not marked with 'try' and the error is not handled}}
  let _ = await invoke(fn: throwingTask)
  let _ = await invoke(fn: normalTask) // ok

  let _ = await asyncRethrows(fn: normalTask)
  let _ = await try! asyncRethrows(fn: normalTask) // expected-warning{{no calls to throwing functions occur within 'try' expression}}
  let _ = await try? asyncRethrows(fn: normalTask) // expected-warning{{no calls to throwing functions occur within 'try' expression}}
  
  let _ = await try! asyncRethrows(fn: throwingTask)
  let _ = await try? asyncRethrows(fn: throwingTask)
  let _ = await try! asyncThrows()
  let _ = await try? asyncThrows()

  //////////
  // some auto-closure tests

  let _ = await invokeAuto("intuitive")
  let _ = await try! invokeAuto(await throwingTask())
  let _ = await try? invokeAuto(await throwingTask())
  let _ = await invokeAuto(await try! throwingTask())
  let _ = await invokeAuto(await try? throwingTask())

  let _ = await invokeAuto(try! throwingTask()) // expected-error{{call is 'async' in an autoclosure argument that is not marked with 'await'}}
  let _ = await invokeAuto(try? throwingTask()) // expected-error{{call is 'async' in an autoclosure argument that is not marked with 'await'}}

  let _ = await invokeAuto(await try! throwingTask())
  let _ = await invokeAuto(await try? throwingTask())
  /////////

  do {
    let _ = await try asyncThrows()
    let _ = await try asyncRethrows(fn: throwingTask)

    //////
    // more auto-closure tests
    
    // expected-note@+6 {{did you mean to disable error propagation?}}
    // expected-note@+5 {{did you mean to handle error as optional value?}}
    // expected-note@+4 {{did you mean to use 'try'?}}
    // expected-note@+3 {{call is to 'rethrows' function, but argument function can throw}}
    // expected-error@+2 {{call is 'async' in an autoclosure argument that is not marked with 'await'}}
    // expected-error@+1 2 {{call can throw but is not marked with 'try'}}
    let _ = await invokeAuto(throwingTask())

    let _ = await try invokeAuto(throwingTask()) // expected-error{{call is 'async' in an autoclosure argument that is not marked with 'await'}}
    let _ = try invokeAuto(await throwingTask()) // expected-error{{call is 'async' but is not marked with 'await'}}
    let _ = await try invokeAuto(await throwingTask())
  } catch {
    // ignore
  }
}