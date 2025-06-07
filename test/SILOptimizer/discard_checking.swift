// RUN: %target-swift-emit-sil -sil-verify-all -verify %s -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency
// REQUIRES: swift_in_compiler

enum Color {
  case red, green, blue, none
}
enum E: Error {
  case someError
}

func globalConsumingFn(_ b: consuming Basics) {}
func globalThrowingFn() throws {}

struct Basics: ~Copyable {
  consuming func test1(_ b: Bool) {
    guard b else {
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    }
    discard self // expected-note {{discarded self here}}
  }

  consuming func test1_fixed(_ b: Bool) {
    guard b else {
      _ = consume self
      return
    }
    discard self
  }

  consuming func test2(_ c: Color) throws {
    repeat {
      switch c {
      case .red:
        return
      case .blue:
        throw E.someError
      case .green:
        self = Basics()
      default: print("hi")
      }
    } while true
    discard self
  }

  consuming func test2_fixed(_ c: Color) throws {
    repeat {
      switch c {
      case .red:
        discard self
        return
      case .blue:
        discard self
        throw E.someError
      case .green:
        self = Basics()
      default: print("hi")
      }
    } while true
    discard self
  }

  consuming func test3(_ c: Color) {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
      return
    } else if case .blue = c {
      try! test2(c)
      return
    } else if case .green = c {
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    } else {
      _ = consume self
    }
  }

  consuming func test3_fixed(_ c: Color) {
    if case .red = c {
      discard self
      return
    } else if case .blue = c {
      try! test2(c)
      return
    } else if case .green = c {
      _ = consume self
      return
    } else {
      _ = consume self
    }
  }

  consuming func test4(_ c: Color) {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
    }
  } // expected-error {{must consume 'self' before exiting method that discards self}}

  consuming func test4_fixed(_ c: Color) {
    if case .red = c {
      discard self
    } else {
      _ = consume self
    }
  }

  consuming func test5(_ c: Color) {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
    } else {
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    }
  }

  consuming func test5_fixed(_ c: Color) {
    if case .red = c {
      discard self
    } else {
      discard self
      return
    }
  }

  // TODO: rdar://110239743 (diagnostic locations for missing consume-before-exit in discarding methods are wonky)
  consuming func test6(_ c: Color) throws {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
    } else {
      mutator()
      throw E.someError // expected-error {{must consume 'self' before exiting method that discards self}}
    }
  }

  consuming func test6_fixed(_ c: Color) throws {
    if case .red = c {
      discard self
    } else {
      mutator()
      globalConsumingFn(self)
      throw E.someError
    }
  }

  consuming func test7(_ c: Color) throws {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
    }
    return // expected-error {{must consume 'self' before exiting method that discards self}}
  }

  consuming func test7_fixed(_ c: Color) throws {
    if case .red = c {
      discard self
      return
    }
    _ = consume self
    return
  }

  consuming func test8(_ c: Color) throws {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
    }
    if case .blue = c {
      return
    }
  } // expected-error {{must consume 'self' before exiting method that discards self}}

  consuming func test8_stillMissingAConsume1(_ c: Color) throws {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
      return
    }
    if case .blue = c {
      _ = consume self
      return
    }
  } // expected-error {{must consume 'self' before exiting method that discards self}}

  consuming func test8_stillMissingAConsume2(_ c: Color) throws {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
      return
    }
    if case .blue = c {
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    }
    _ = consume self
  }

  consuming func test8_fixed(_ c: Color) throws {
    if case .red = c {
      discard self
      return
    }
    if case .blue = c {
      _ = consume self
      return
    }
    _ = consume self
  }

  consuming func test9(_ c: Color) throws {
    if case .red = c {
      discard self // expected-note {{discarded self here}}
      return
    }

    do {
      throw E.someError
    } catch E.someError {
      try test8(c)
      return
    } catch {
      print("hi")
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    }
    _ = consume self // expected-warning {{will never be executed}}
  }

  consuming func test9_fixed(_ c: Color) throws {
    if case .red = c {
      discard self
      return
    }

    do {
      throw E.someError
    } catch E.someError {
      try test8(c)
      return
    } catch {
      print("hi")
      _ = consume self
      return
    }
    _ = consume self // expected-warning {{will never be executed}}
  }

  consuming func test10(_ c: Color) throws {
    if case .red = c {
      discard self // expected-note 2{{discarded self here}}
      return
    }

    do {
      throw E.someError
    } catch E.someError {
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    } catch {
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    }
  }

  consuming func test11(_ c: Color) {
    guard case .red = c else {
      discard self // expected-note {{discarded self here}}
      return
    }
    defer { print("hi") }
    mutator()
    _ = consume self
    self = Basics()
    borrower()
    let x = self
    self = x
    mutator()
  } // expected-error {{must consume 'self' before exiting method that discards self}}

  consuming func test11_fixed(_ c: Color) {
    guard case .red = c else {
      discard self
      return
    }
    defer { print("hi") }
    mutator()
    _ = consume self
    self = Basics()
    borrower()
    let x = self
    self = x
    mutator()
    discard self
  }

  consuming func test12(_ c: Color) throws {
    guard case .red = c else {
      discard self // expected-note {{discarded self here}}
      return
    }
    try thrower() // expected-error {{must consume 'self' before exiting method that discards self}}
    print("hi")
    _ = consume self
  }

  consuming func test12_fixed1(_ c: Color) throws {
    guard case .red = c else {
      discard self
      return
    }
    try? thrower()
    print("hi")
    _ = consume self
  }

  consuming func test12_fixed2(_ c: Color) throws {
    guard case .red = c else {
      discard self
      return
    }
    do {
      try thrower()
    } catch {
      print("hi")
      _ = consume self
      throw error
    }
    print("hi")
    _ = consume self
  }

  consuming func test12Bang(_ c: Color) throws {
    guard case .red = c else {
      discard self // expected-note {{discarded self here}}
      return
    }
    try! thrower() // expected-error {{must consume 'self' before exiting method that discards self}}
    print("hi")
    _ = consume self
  }

  consuming func test13(_ c: Color) async {
    guard case .red = c else {
      discard self // expected-note {{discarded self here}}
      return
    }
    await asyncer()
  } // expected-error {{must consume 'self' before exiting method that discards self}}

  consuming func test13_fixed(_ c: Color) async {
    guard case .red = c else {
      discard self
      return
    }
    await asyncer()
    _ = consume self
  }

  consuming func test14(_ c: Color) async {
    guard case .red = c else {
      discard self // expected-note {{discarded self here}}
      return
    }
    await withCheckedContinuation { cont in
      cont.resume()
    }
    print("back!")
  } // expected-error {{must consume 'self' before exiting method that discards self}}

  consuming func test14_fixed(_ c: Color) async {
    guard case .red = c else {
      discard self
      return
    }
    await withCheckedContinuation { cont in
      cont.resume()
    }
    print("back!")
    _ = consume self
  }

  consuming func positiveTest(_ i: Int) {
    switch i {
    case 0: _ = self
    case 1: let _ = self
    case 2:
      let other = self
      _ = other
    case 3:
      _ = consume self
    case 4:
      self.test11(.red)
    case 5:
      globalConsumingFn(self)
    default:
      discard self
    }
  }

  // FIXME move checker is treating the defer like a closure capture (rdar://100468597)
  // not expecting any errors here
  consuming func brokenPositiveTest(_ i: Int) { // expected-error {{missing reinitialization of closure capture 'self' after consume}}
    defer { discard self } // expected-note {{consumed here}}
    switch i {
    case 0, 1, 2, 3: return
    default:
      break
    }
  }

  consuming func negativeTest(_ i: Int) throws {
    switch i {
    case 0:
      fallthrough
    case 1:
      throw E.someError // expected-error {{must consume 'self' before exiting method that discards self}}
    case 2:
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    case 3:
      return // expected-error {{must consume 'self' before exiting method that discards self}}
    case 4:
      globalConsumingFn(self)
    default:
      discard self // expected-note 3{{discarded self here}}
    }
  }

  consuming func loopyExit_bad(_ i: Int) {
    if i < 0 {
      discard self // expected-note {{discarded self here}}
      return
    }

    for _ in 0..<i {
      self = Basics()
    }

    return // expected-error {{must consume 'self' before exiting method that discards self}}
  }

  consuming func loopyExit_good(_ i: Int) {
    if i < 0 {
      discard self
      return
    }

    for _ in 0..<i {
      self = Basics()
    }

    _ = consume self
    return
  }


  ////////////////////////////////////////
  /// Checking for reinit-after-discard
  ////////////////////////////////////////
  consuming func reinitAfterDiscard1(_ c: Color) throws {
    discard self // expected-note {{discarded self here}}
    self = Basics() // expected-error {{cannot reinitialize 'self' after 'discard self'}}
    _ = consume self
  }

  consuming func reinitAfterDiscard2_bad(_ c: Color) throws {
    discard self // expected-note {{discarded self here}}

    if case .red = c {
      throw E.someError
    }

    self = Basics() // expected-error {{cannot reinitialize 'self' after 'discard self'}}
    _ = consume self
  }

  consuming func reinitAfterDiscard2_ok(_ c: Color) throws {
    if case .red = c {
      discard self
      throw E.someError
    }

    self = Basics()
    _ = consume self
  }

  consuming func reinitAfterDiscard3_bad(_ c: Color) throws {
    repeat {
      self = Basics() // expected-error {{cannot reinitialize 'self' after 'discard self'}}
      discard self // expected-note 2{{discarded self here}}
    } while 1 == 0
  } // expected-error {{must consume 'self' before exiting method that discards self}}

  consuming func reinitAfterDiscard3_ok1(_ c: Color) throws {
    repeat {
      self = Basics()
      discard self
    } while false
  }

  consuming func reinitAfterDiscard3_ok2(_ c: Color) throws {
    self = Basics()
    discard self
  }

  consuming func reinitAfterDiscard4_bad(_ c: Color) throws {
    do {
      discard self // expected-note {{discarded self here}}
      if case .red = c {
        try globalThrowingFn()
      }
    } catch {
      self = Basics() // expected-error {{cannot reinitialize 'self' after 'discard self'}}
      _ = consume self
    }
  }

  consuming func reinitAfterDiscard4_ok(_ c: Color) throws {
    do {
      if case .red = c {
        try globalThrowingFn()
      }
      discard self
    } catch {
      self = Basics()
      _ = consume self
    }
  }

  consuming func reinitAfterDiscard4_ok_v2(_ c: Color) throws {
    do {
      if case .red = c {
        try? globalThrowingFn()
      }
      discard self
    } catch { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
      self = Basics()
      _ = consume self
    }
  }

  // FIXME move checker is treating the defer like a closure capture (rdar://100468597)
  // There is a different expected error here, where this reinit in a defer
  // should be illegal because it's a reinit after discard.
  consuming func reinitAfterDiscard5(_ c: Color) throws {
    // expected-error@-1 {{'self' used after consume}}

    defer { self = Basics() } // expected-note {{used here}}
    discard self // expected-note {{consumed here}}
  }

  consuming func reinitAfterDiscard6(_ c: Color) throws {
    self = Basics()
    discard self // expected-note 3{{discarded self here}}
    switch c {
    case .red, .blue:
      self = Basics()  // expected-error {{must consume 'self' before exiting method that discards self}}
    default:
      self = Basics()  // expected-error {{must consume 'self' before exiting method that discards self}}
                       // expected-error@-1 {{cannot reinitialize 'self' after 'discard self'}}
    }
  }

  mutating func mutator() { self = Basics() }
  borrowing func borrower() {}
  borrowing func thrower() throws {}

  @MainActor borrowing func asyncer() async {}

  deinit { print("hi") }
}

struct Money: ~Copyable {
  enum Error: Swift.Error {
    case insufficientFunds
  }

  let balance: Int

  consuming func spend(_ charge: Int) throws -> Money {
    guard charge > 0 else {
      return Money(balance: balance) // expected-error {{must consume 'self' before exiting method that discards self}}
    }

    if balance < charge  {
      throw Error.insufficientFunds // expected-error {{must consume 'self' before exiting method that discards self}}
    } else if balance > charge {
      self = Money(balance: balance - charge)
      return self
    }

    discard self // expected-note 2{{discarded self here}}
    return Money(balance: 0)
  }

  deinit {
    assert(balance > 0)
  }
}
