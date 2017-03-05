// RUN: %target-swift-frontend -typecheck -verify -module-name main %s

/** Basics *******************************************************************/

// Function types can't be rethrows right now.
let r1 = {() rethrows -> Int in 0} // expected-error {{only function declarations may be marked 'rethrows'; did you mean 'throws'?}} {{14-22=throws}}
let r2 : () rethrows -> Int = { 0 } // expected-error {{only function declarations may be marked 'rethrows'; did you mean 'throws'?}} {{13-21=throws}}
let r3 : Optional<() rethrows -> ()> = nil // expected-error {{only function declarations may be marked 'rethrows'; did you mean 'throws'?}} {{22-30=throws}}

func f1(_ f: () throws -> ()) rethrows { try f() }
func f2(_ f: () -> ()) rethrows { f() } // expected-error {{'rethrows' function must take a throwing function argument}}
func f3(_ f: UndeclaredFunctionType) rethrows { f() } // expected-error {{use of undeclared type 'UndeclaredFunctionType'}}

/** Protocol conformance checking ********************************************/

protocol P {
  func tf() throws
  func nf() // expected-note {{protocol requires}}

  func thf(_ f: () throws -> ()) throws
  func nhf(_ f: () throws -> ()) // expected-note 2 {{protocol requires}}
  func rhf(_ f: () throws -> ()) rethrows // expected-note {{protocol requires}}
}

struct T0 : P { // expected-error {{type 'T0' does not conform to protocol 'P'}}
  func tf() throws {}
  func nf() throws {} // expected-note {{candidate throws, but protocol does not allow it}}

  func thf(_ f: () throws -> ()) throws {}
  func nhf(_ f: () throws -> ()) throws {} // expected-note {{candidate throws, but protocol does not allow it}}
  func rhf(_ f: () throws -> ()) throws {} // expected-note {{candidate is not 'rethrows', but protocol requires it}}
}

struct T1 : P {
  func tf() {}
  func nf() {}

  func thf(_ f: () throws -> ()) {}
  func nhf(_ f: () throws -> ()) {}
  func rhf(_ f: () throws -> ()) {}
}

struct T2 : P { // expected-error {{type 'T2' does not conform to protocol 'P'}}
  func tf() {}
  func nf() {}

  func thf(_ f: () throws -> ()) rethrows {}
  func nhf(_ f: () throws -> ()) rethrows {} // expected-note {{candidate throws, but protocol does not allow it}}
  func rhf(_ f: () throws -> ()) rethrows {}
}

/** Classes ******************************************************************/

class Super {
  func tf() throws {}
  func nf() {} // expected-note {{overridden declaration is here}}

  func thf(_ f: () throws -> ()) throws {}
  func nhf(_ f: () throws -> ()) {} // expected-note 2 {{overridden declaration is here}}
  func rhf(_ f: () throws -> ()) rethrows {} // expected-note {{overridden declaration is here}}
}

class C1 : Super {
  override func tf() {}
  override func nf() {}

  override func thf(_ f: () throws -> ()) {}
  override func nhf(_ f: () throws -> ()) {}
  override func rhf(_ f: () throws -> ()) {}
}

class C2 : Super {
  override func tf() throws {}
  override func nf() throws {} // expected-error {{cannot override non-throwing method with throwing method}}

  override func thf(_ f: () throws -> ()) throws {}
  override func nhf(_ f: () throws -> ()) throws {} // expected-error {{cannot override non-throwing method with throwing method}}
  override func rhf(_ f: () throws -> ()) throws {} // expected-error {{override of 'rethrows' method should also be 'rethrows'}}
}

class C3 : Super {
  override func tf() {}
  override func nf() {}

  override func thf(_ f: () throws -> ()) rethrows {}
  override func nhf(_ f: () throws -> ()) rethrows {} // expected-error {{cannot override non-throwing method with throwing method}}
  override func rhf(_ f: () throws -> ()) rethrows {}
}

/** Semantics ****************************************************************/

@discardableResult
func call(_ fn: () throws -> Int) rethrows -> Int { return try fn() }
@discardableResult
func callAC(_ fn: @autoclosure () throws -> Int) rethrows -> Int { return try fn() }
@discardableResult
func raise() throws -> Int { return 0 }
@discardableResult
func noraise() -> Int { return 0 }

/** Global functions **/

func testCallUnhandled() {
  call(noraise)
  try call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  call(raise) // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try call(raise) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testCallHandled() throws {
  call(noraise)
  try call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  call(raise) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try call(raise)
}

func testCallACUnhandled() {
  callAC(noraise())
  try callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  callAC(raise()) // expected-error {{call can throw but is not marked with 'try'}} \
                  // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} \
                  // expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try callAC(raise()) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testCallACHandled() throws {
  callAC(noraise())
  try callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  callAC(raise()) // expected-error 2 {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try callAC(raise())
}

func testForward1(_ fn: () throws -> Int) rethrows {
  try call(fn)
}
func testForward2(_ fn: () throws -> Int) rethrows {
  try call({ try fn() })
}

/** Methods **/

struct MyStruct : MyProto {
  @discardableResult
  func call(_ fn: () throws -> Int) rethrows -> Int { return try fn() }
  @discardableResult
  func callAC(_ fn: @autoclosure () throws -> Int) rethrows -> Int { return try fn() }

  @discardableResult
  static func static_call(_ fn: () throws -> Int) rethrows -> Int { return try fn() }
  @discardableResult
  static func static_callAC(_ fn: @autoclosure () throws -> Int) rethrows -> Int { return try fn() }
}

func testMethodCallUnhandled(_ s: MyStruct) {
  s.call(noraise)
  try s.call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.call(raise) // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.call(raise) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}

  MyStruct.static_call(noraise)
  try MyStruct.static_call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  MyStruct.static_call(raise) // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try MyStruct.static_call(raise) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testMethodCallHandled(_ s: MyStruct) throws {
  s.call(noraise)
  try s.call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.call(raise) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.call(raise)

  MyStruct.static_call(noraise)
  try MyStruct.static_call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  MyStruct.static_call(raise) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try MyStruct.static_call(raise)
}

func testMethodCallACUnhandled(_ s: MyStruct) {
  s.callAC(noraise())
  try s.callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.callAC(raise()) // expected-error {{call can throw but is not marked with 'try'}} \
                  // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} \
                  // expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.callAC(raise()) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}

  MyStruct.static_callAC(noraise())
  try MyStruct.static_callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  MyStruct.static_callAC(raise()) // expected-error {{call can throw but is not marked with 'try'}} \
                  // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} \
                  // expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try MyStruct.static_callAC(raise()) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testMethodCallACHandled(_ s: MyStruct) throws {
  s.callAC(noraise())
  try s.callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.callAC(raise()) // expected-error 2 {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.callAC(raise())

  MyStruct.static_callAC(noraise())
  try MyStruct.static_callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  MyStruct.static_callAC(raise()) // expected-error 2 {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try MyStruct.static_callAC(raise())
}

/** Protocol methods **/

protocol MyProto {
  @discardableResult
  func call(_ fn: () throws -> Int) rethrows -> Int
  @discardableResult
  func callAC(_ fn: @autoclosure () throws -> Int) rethrows -> Int

  @discardableResult
  static func static_call(_ fn: () throws -> Int) rethrows -> Int
  @discardableResult
  static func static_callAC(_ fn: @autoclosure () throws -> Int) rethrows -> Int
}

/** Existentials **/

func testProtoMethodCallUnhandled(_ s: MyProto) {
  s.call(noraise)
  try s.call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.call(raise) // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.call(raise) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}

  type(of: s).static_call(noraise)
  try type(of: s).static_call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  type(of: s).static_call(raise) // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try type(of: s).static_call(raise) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testProtoMethodCallHandled(_ s: MyProto) throws {
  s.call(noraise)
  try s.call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.call(raise) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.call(raise)

  type(of: s).static_call(noraise)
  try type(of: s).static_call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  type(of: s).static_call(raise) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try type(of: s).static_call(raise)
}

func testProtoMethodCallACUnhandled(_ s: MyProto) {
  s.callAC(noraise())
  try s.callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.callAC(raise()) // expected-error {{call can throw but is not marked with 'try'}} \
                  // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} \
                  // expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.callAC(raise()) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}

  type(of: s).static_callAC(noraise())
  try type(of: s).static_callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  type(of: s).static_callAC(raise()) // expected-error {{call can throw but is not marked with 'try'}} \
                  // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} \
                  // expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try type(of: s).static_callAC(raise()) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testProtoMethodCallACHandled(_ s: MyProto) throws {
  s.callAC(noraise())
  try s.callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.callAC(raise()) // expected-error 2 {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.callAC(raise())

  type(of: s).static_callAC(noraise())
  try type(of: s).static_callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  type(of: s).static_callAC(raise()) // expected-error 2 {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try type(of: s).static_callAC(raise())
}

/** Generics **/

func testGenericMethodCallUnhandled<P: MyProto>(_ s: P) {
  s.call(noraise)
  try s.call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.call(raise) // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.call(raise) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}

  P.static_call(noraise)
  try P.static_call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  P.static_call(raise) // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try P.static_call(raise) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testGenericMethodCallHandled<P: MyProto>(_ s: P) throws {
  s.call(noraise)
  try s.call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.call(raise) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.call(raise)

  P.static_call(noraise)
  try P.static_call(noraise) // expected-warning {{no calls to throwing functions occur within 'try'}}
  P.static_call(raise) // expected-error {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try P.static_call(raise)
}

func testGenericMethodCallACUnhandled<P: MyProto>(_ s: P) {
  s.callAC(noraise())
  try s.callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.callAC(raise()) // expected-error {{call can throw but is not marked with 'try'}} \
                  // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} \
                  // expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.callAC(raise()) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}

  P.static_callAC(noraise())
  try P.static_callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  P.static_callAC(raise()) // expected-error {{call can throw but is not marked with 'try'}} \
                  // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled}} \
                  // expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try P.static_callAC(raise()) // expected-error {{call can throw, but the error is not handled}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
}

func testGenericMethodCallACHandled<P: MyProto>(_ s: P) throws {
  s.callAC(noraise())
  try s.callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  s.callAC(raise()) // expected-error 2 {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try s.callAC(raise())

  P.static_callAC(noraise())
  try P.static_callAC(noraise()) // expected-warning {{no calls to throwing functions occur within 'try'}}
  P.static_callAC(raise()) // expected-error 2 {{call can throw but is not marked with 'try'}} expected-note {{call is to 'rethrows' function, but argument function can throw}}
  try P.static_callAC(raise())
}

/** Optional closure parameters */

func testForceUnwrappedOptionalFunctionParameter(_ f: (() throws -> Void)?) rethrows {
  try f!()
}

func testBindOptionalFunctionParameter(_ f: (() throws -> Void)?) rethrows {
  try f?()
}

func testImplicitlyUnwrappedFunctionParameter(_ f: (() throws -> Void)!) rethrows {
  if f != nil {
    try f()
  }
}

func throwingFunc() throws {}

func nonThrowingFunc() {}

try testBindOptionalFunctionParameter(throwingFunc)
testBindOptionalFunctionParameter(nonThrowingFunc)
testBindOptionalFunctionParameter(nil)

try testImplicitlyUnwrappedFunctionParameter(throwingFunc)
testImplicitlyUnwrappedFunctionParameter(nonThrowingFunc)
testImplicitlyUnwrappedFunctionParameter(nil)


/** Miscellaneous bugs **/

// rdar://problem/21967164 - Non-throwing closures are incorrectly marked as throwing in rethrow contexts
func rt1(predicate: () throws -> ()) rethrows { }
rt1 { }

func rt2(_ predicate: () throws -> ()) rethrows { }
rt2 { }


enum SomeError : Error {
  case Badness
}

func testUnrelatedThrowsInRethrows(_ fn: () throws -> Void) rethrows {
  try fn() // okay
  try testUnrelatedThrowsInRethrows(fn) // okay

  raise() // expected-error {{call can throw, but it is not marked with 'try' and the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
  try raise() // expected-error {{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
  throw SomeError.Badness // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
}

func testThrowsInCatchInRethrows(_ fn: () throws -> Void) rethrows {
  do {
    try fn()
  } catch {
    // this catch can only be entered if our `fn` parameter throws
    throw error // okay
  }

  do {
    try fn()
  } catch let error as SomeError {
    throw error // okay
  }

  do {
    try fn()
    try raise()
  } catch {
    // this catch can be entered regardless of whether our `fn` parameter throws
    throw error // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }

  do {
    throw SomeError.Badness
  } catch {
    // this catch can be entered regardless of whether our `fn` parameter throws
    throw error // expected-error {{a function declared 'rethrows' may only throw if its parameter does}}
  }

  do {
    try fn()
    try raise() // expected-error {{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
  } catch is SomeError {}

  do {
    try raise()
  } catch {
    try fn() // okay
  }

  do {
    // don't throw anything; equivalent to throwing in an #if
  } catch { // expected-warning {{'catch' block is unreachable because no errors are thrown in 'do' block}}
    throw error
  }
}

// Sanity-check that throwing in catch blocks behaves as expected outside of
// rethrows functions

func testThrowsInCatch(_ fn: () throws -> Void) {
  do {
    try fn()
  } catch {
    throw error // expected-error {{error is not handled because the enclosing function is not declared 'throws'}}
  }
}

func testThrowsInCatchInThrows() throws {
  do {
    try raise()
  } catch {
    throw error // okay
  }
}

// <rdar://problem/24221830> Bogus "no calls to throwing functions" warning in derived throwing init
class B24221830 {}
class r24221830 : B24221830 {
  var B: Int
  
  init(A: String) throws {
    self.B = 0
  }
  
}

// rdar://problem/30618853

func gallant(_: () throws -> ()) rethrows {}

func goofus(_ f: () -> ()) {
  gallant(f)
  main.gallant(f)
}

func goofus(_ f: () throws -> ()) rethrows {
  try gallant(f)
  try main.gallant(f)
}

struct Foo {
  func foo() {}
}

func throwWhileGettingFoo() throws -> Foo.Type { return Foo.self }

(throwWhileGettingFoo()).foo(Foo())() // expected-error {{can throw}}
(try throwWhileGettingFoo()).foo(Foo())()
