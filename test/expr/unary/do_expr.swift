// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature ThenStatements -enable-experimental-feature DoExpressions

// REQUIRES: swift_feature_DoExpressions
// REQUIRES: swift_feature_ThenStatements

@discardableResult
func throwsError() throws -> Int { 0 }

struct Err: Error {}

func test1() -> Int {
  return do { 5 }
}

func test2() -> Int {
  return do { try throwsError() } catch { 0 }
}

func test3() -> Int {
  return
  do { 5 }
  // expected-warning@-1 {{expression following 'return' is treated as an argument of the 'return'}}
  // expected-note@-2 {{indent the expression to silence this warning}}
}

func test4() -> Int {
  return
    do { 5 }
}

func test5() -> Int {
  return
  do { try throwsError() } catch { 0 }
  // expected-warning@-1 {{expression following 'return' is treated as an argument of the 'return'}}
  // expected-note@-2 {{indent the expression to silence this warning}}
}

func test6() -> Int {
  return
    do { try throwsError() } catch { 0 }
}

func test7() -> Int {
  do { 5 }
}

func test8() -> Int {
  do { try throwsError() } catch { 0 }
}

func test9() -> Int {
  do { 5 } as Int
}

func test10() -> Int {
  do { try throwsError() } catch { 0 } as Int
}

func test11() -> Int {
  let x = do { 5 }
  return x
}

func test12() -> Int {
  let x = do { try throwsError() } catch { 0 }
  return x
}

func test13() -> Int {
  let fn = { do { 5 } }
  return fn()
}

func test14() -> Int {
  let fn = { do { try throwsError() } catch { 0 } }
  return fn()
}

func test15() -> Int {
  let x = if .random() {
    do { 0 }
  } else {
    1
  }
  return x
}

func test16() -> Int {
  let x = if .random() {
    1
  } else {
    do { 2 } catch { 3 }
    // expected-warning@-1 {{'catch' block is unreachable because no errors are thrown in 'do' block}}
  }
  return x
}

func test17() -> Int {
  if .random() {
    do { 0 }
  } else {
    1
  }
}

func test18() -> Int {
  if .random() {
    1
  } else {
    do { 2 } catch { 3 }
    // expected-warning@-1 {{'catch' block is unreachable because no errors are thrown in 'do' block}}
  }
}

func testEmpty1() {
  let _ = do {} // expected-error {{expected expression in branch of 'do' expression}}
}

func testEmpty2() -> Int {
  // Fine, treated as a statement.
  do {}
}

func testEmpty3() -> Int {
  let _ = do { try throwsError() } catch {}
  // expected-error@-1 {{expected expression in branch of 'do-catch' expression}}
}

func testEmpty4() -> Int {
  // Fine, treated as a statement.
  do { try throwsError() } catch {}
}

func testNonExhaustive1() throws -> Int {
  let _ = do { try throwsError() } catch is Err { 0 }
  // expected-error@-1 {{'do catch' must have an unconditional 'catch' to be used as expression}}
}

func testNonExhaustive2() throws -> Int {
  // Non-exhaustive, so statement.
  do { try throwsError() } catch is Err { 0 }
  // expected-warning@-1 {{integer literal is unused}}
}

func testReturn1() -> Int {
  let _ = do {
    try throwsError()
  } catch {
    if .random() {
      return 0 // expected-error {{cannot use 'return' to transfer control out of 'do-catch' expression}}
    }
    then 0
  }
}

func testReturn2() -> Int {
  // The return means this must be a statement.
  do {
    try throwsError()
  } catch {
    if .random() {
      return 0
    }
    then 0 // expected-error {{'then' may only appear as the last statement in an 'if', 'switch', or 'do' expression}}
  }
}

// MARK: Effect specifiers

func tryDo1() -> Int {
  try do { 0 }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
}

func tryDo2() -> Int {
  let x = try do { 0 }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  return x
}

func tryDo3() -> Int {
  return try do { 0 }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
}

func tryDo4() throws -> Int {
  return try do { 0 }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
}

func tryDo5() throws -> Int {
  return try do { tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
}

func tryDo6() throws -> Int {
  try do { tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
}

func tryDo7() throws -> Int {
  let x = try do { tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
  return x
}

func tryDo8() throws -> Int {
  return try do { try tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
}

func tryDo9() throws -> Int {
  try do { try tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
}

func tryDo10() throws -> Int {
  let x = try do { try tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  return x
}

func tryDo11() throws -> Int {
  let x = try do { try tryDo4() } catch { tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do-catch' expression}}
  // expected-warning@-2 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{did you mean to use 'try'?}}
  // expected-note@-4 {{did you mean to handle error as optional value?}}
  // expected-note@-5 {{did you mean to disable error propagation?}}
  return x
}

func tryDo12() throws -> Int {
  let x = try do { tryDo4() } catch { tryDo4() }
  // expected-warning@-1 {{'try' has no effect on 'do-catch' expression}}
  // expected-warning@-2 2{{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 2{{did you mean to use 'try'?}}
  // expected-note@-4 2{{did you mean to handle error as optional value?}}
  // expected-note@-5 2{{did you mean to disable error propagation?}}
  return x
}

func tryDo13() throws -> Int {
  let x = try do { // expected-warning {{'try' has no effect on 'do-catch' expression}}
    tryDo4() // expected-warning {{result of call to 'tryDo4()' is unused}}
    // expected-warning@-1 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{did you mean to use 'try'?}}
    // expected-note@-3 {{did you mean to handle error as optional value?}}
    // expected-note@-4 {{did you mean to disable error propagation?}}

    _ = tryDo4()
    // expected-warning@-1 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{did you mean to use 'try'?}}
    // expected-note@-3 {{did you mean to handle error as optional value?}}
    // expected-note@-4 {{did you mean to disable error propagation?}}

    _ = try tryDo4() // Okay.

    // Okay.
    do {
      _ = try tryDo4()
    } catch {}

    print("hello")
    throw Err()
  } catch _ where .random() {
    0
  } catch {
    throw error
  }
  return x
}

func throwsBool() throws -> Bool { true }

func tryDo14() throws -> Int {
  try do { try tryDo4() } catch _ where throwsBool() { 0 } catch { 1 }
  // expected-warning@-1 {{'try' has no effect on 'do-catch' expression}}
  // expected-error@-2 {{call can throw, but errors cannot be thrown out of a catch guard expression}}
}

func tryDo15() throws -> Int {
  try do { try tryDo4() } catch _ where try throwsBool() { 1 } catch { 1 }
  // expected-warning@-1 {{'try' has no effect on 'do-catch' expression}}
  // expected-error@-2 {{call can throw, but errors cannot be thrown out of a catch guard expression}}
}

func tryDo16() throws -> Int {
  do { try tryDo4() } catch _ where throwsBool() { 0 } catch { 1 }
  // expected-error@-1 {{call can throw, but errors cannot be thrown out of a catch guard expression}}
}

func tryDo17() throws -> Int {
  do { tryDo4() } catch { 1 }
  // expected-error@-1 {{call can throw but is not marked with 'try'}}
  // expected-note@-2 {{did you mean to use 'try'?}}
  // expected-note@-3 {{did you mean to handle error as optional value?}}
  // expected-note@-4 {{did you mean to disable error propagation?}}
}

func tryDo18() {
  // Make sure we don't warn here.
  do {
    let _ = do { try tryDo4() }
  } catch {}
}

func tryDo19() {
  // Make sure we don't warn here.
  do {
    let _ = do { throw Err() }
  } catch {}
}

func tryDo19() throws -> Int {
  let x = do { try tryDo4() } catch { throw Err() }
  return x
}

func tryDo20() throws -> Int {
  do { try tryDo4() } catch { throw Err() }
}

func tryDo21(_ fn: () throws -> Int) rethrows -> Int {
  let x = do { try fn() }
  return x
}

func tryDo22(_ fn: () throws -> Int) rethrows -> Int {
  do { try fn() }
}

func tryDo23(_ fn: () throws -> Int) rethrows -> Int {
  // Fine, we can only end up in the 'catch' if we rethrow in the first place.
  let x = do { try fn() } catch { throw Err() }
  return x
}

func tryDo23_2(_ fn: () throws -> Int) rethrows -> Int {
  let x = do { try tryDo4() } catch _ where .random() { try fn() } catch { throw Err() }
  // expected-error@-1 {{a function declared 'rethrows' may only throw if its parameter does}}
  return x
}

func tryDo24(_ fn: () throws -> Int) rethrows -> Int {
  // Fine, we can only end up in the 'catch' if we rethrow in the first place.
  let x = do { try fn() } catch { try tryDo4() }
  return x
}

func tryDo24_2(_ fn: () throws -> Int) rethrows -> Int {
  let x = do { try tryDo4() } catch _ where .random() { try fn() } catch { try tryDo4() }
  // expected-error@-1 {{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
  return x
}

func tryDo25(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = do { try fn() } catch { try tryDo4() }
    return x
  } catch {
    return 0
  }
}

func tryDo26(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = do { try fn() } catch { throw Err() }
    return x
  } catch {
    return 0
  }
}

func tryDo27(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = do { try fn() } catch { try tryDo4() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryDo28(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = do { try fn() } catch { throw Err() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryDo29(_ fn: () throws -> Int) rethrows -> Int {
  do {
    let x = do { try fn() }
    return x
  } catch {
    throw error // Okay.
  }
}

func tryDo30(_ fn: () throws -> Int) rethrows -> Int {
  // FIXME: This ought to work (https://github.com/apple/swift/issues/68824)
  do {
    let x = do { try fn() } catch { throw error }
    return x
  } catch {
    throw error // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func awaitDo1() async -> Int {
  await do { 0 }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
}

func awaitDo2() async -> Int {
  let x = await do { 0 }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
  return x
}

func awaitDo3() -> Int { // expected-note {{add 'async' to function 'awaitDo3()' to make it asynchronous}}
  return await do { 0 }
  // expected-error@-1 {{'await' in a function that does not support concurrency}}
}

func awaitDo4() async -> Int {
  return await do { 0 }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
}

func awaitDo5() async -> Int {
  return await do { awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@-3 {{call is 'async'}}
}

func awaitDo6() async -> Int {
  await do { awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@-3 {{call is 'async'}}
}

func awaitDo7() async -> Int {
  let x = await do { awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@-3 {{call is 'async'}}
  return x
}

func awaitDo8() async -> Int {
  return await do { await awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
}

func awaitDo9() async -> Int {
  await do { await awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
}

func awaitDo10() async -> Int {
  let x = await do { await awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do' expression}}
  return x
}

func awaitDo11() async -> Int {
  let x = await do { try await tryAwaitDo1() } catch { awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do-catch' expression}}
  // expected-warning@-2 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 {{call is 'async'}}
  return x
}

func awaitDo12() async -> Int {
  let x = await do { try tryAwaitDo1() } catch { awaitDo4() }
  // expected-warning@-1 {{'await' has no effect on 'do-catch' expression}}
  // expected-warning@-2 2{{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-3 2{{call is 'async'}}
  return x
}

func awaitDo13() async throws -> Int {
  let x = await do { // expected-warning {{'await' has no effect on 'do-catch' expression}}
    awaitDo4() // expected-warning {{result of call to 'awaitDo4()' is unused}}
    // expected-warning@-1 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{call is 'async'}}

    _ = awaitDo4()
    // expected-warning@-1 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
    // expected-note@-2 {{call is 'async'}}

    _ = await awaitDo4() // Okay.

    // Okay.
    let _ = {
      _ = await awaitDo4()
    }

    print("hello")
    throw Err()
  } catch _ where .random() {
    0
  } catch {
    throw error
  }
  return x
}

func asyncBool() async -> Bool { true }

func awaitDo14() async -> Int {
  await do { try tryDo4() } catch _ where asyncBool() { 0 } catch { 1 }
  // expected-warning@-1 {{'await' has no effect on 'do-catch' expression}}
  // expected-error@-2 {{'async' call cannot occur in a catch guard expression}}
}

func awaitDo15() async -> Int {
  await do { try tryDo4() } catch _ where await asyncBool() { 0 } catch { 1 }
  // expected-warning@-1 {{'await' has no effect on 'do-catch' expression}}
  // expected-error@-2 {{'async' call cannot occur in a catch guard expression}}
}

func awaitDo16() async -> Int {
  do { try tryDo4() } catch _ where asyncBool() { 0 } catch { 1 }
  // expected-error@-1 {{'async' call cannot occur in a catch guard expression}}
}

func awaitDo17() async -> Int {
  do { awaitDo4() }
  // expected-error@-1 {{expression is 'async' but is not marked with 'await'}}
  // expected-note@-2 {{call is 'async'}}
}

func awaitDo18() {
  let _ = {
    let _ = do { await awaitDo4() }
  }
}

func awaitDo19() async -> Int {
  let x = do { await awaitDo4() }
  return x
}

func awaitDo20() async -> Int {
  do { await awaitDo4() }
}

func tryAwaitDo1() async throws -> Int {
  try await do { 0 }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
}

func tryAwaitDo2() async throws -> Int {
  try await do { 0 } as Int
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
}

func tryAwaitDo3() async throws -> Int {
  try await do { tryAwaitDo2() } as Int
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
  // expected-warning@-7 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-8 {{call is 'async'}}
}

func tryAwaitDo4() async throws -> Int {
  try await do { try tryAwaitDo2() } as Int
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
  // expected-warning@-3 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{call is 'async'}}
}

func tryAwaitDo5() async throws -> Int {
  try await do { await tryAwaitDo2() } as Int
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
}

func tryAwaitDo6() async throws -> Int {
  try await do { try await tryAwaitDo2() } as Int
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
}

func tryAwaitDo7() async throws -> Int {
  try await do { tryAwaitDo2() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
  // expected-warning@-7 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-8 {{call is 'async'}}
}

func tryAwaitDo8() async throws -> Int {
  try await do { try tryAwaitDo2() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
  // expected-warning@-3 {{expression is 'async' but is not marked with 'await'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{call is 'async'}}
}

func tryAwaitDo9() async throws -> Int {
  try await do { await tryAwaitDo2() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
  // expected-warning@-3 {{call can throw but is not marked with 'try'; this is an error in the Swift 6 language mode}}
  // expected-note@-4 {{did you mean to use 'try'?}}
  // expected-note@-5 {{did you mean to handle error as optional value?}}
  // expected-note@-6 {{did you mean to disable error propagation?}}
}

func tryAwaitDo10() async throws -> Int {
  try await do { try await tryAwaitDo2() }
  // expected-warning@-1 {{'try' has no effect on 'do' expression}}
  // expected-warning@-2 {{'await' has no effect on 'do' expression}}
}

func tryAwaitDo11(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = do { try await fn() } catch { try await tryAwaitDo4() }
    return x
  } catch {
    return 0
  }
}

func tryAwaitDo12(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = do { try await fn() } catch { throw Err() }
    return x
  } catch {
    return 0
  }
}

func tryAwaitDo13(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = do { try await fn() } catch { try await tryAwaitDo4() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryAwaitDo14(_ fn: () async throws -> Int) async rethrows -> Int {
  do {
    let x = do { try await fn() } catch { throw Err() }
    return x
  } catch {
    throw error  // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}

func tryAwaitDo15(_ fn: () async throws -> Int) async rethrows -> Int {
  // FIXME: This ought to work (https://github.com/apple/swift/issues/68824)
  do {
    let x = do { try await fn() } catch { throw error }
    return x
  } catch {
    throw error // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }
}
