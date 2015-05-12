// RUN: %target-parse-verify-swift

/** Basics *******************************************************************/

// Function types can't be rethrows right now.
let r1 = {() rethrows -> Int in 0} // expected-error {{only function declarations may be marked 'rethrows'}}
let r2 : () rethrows -> Int = { 0 } // expected-error {{only function declarations may be marked 'rethrows'}}
let r3 : Optional<() rethrows -> ()> = nil // expected-error {{only function declarations may be marked 'rethrows'}}

func f1(f: () throws -> ()) rethrows { try f() }
func f2(f: () -> ()) rethrows { f() } // expected-error {{'rethrows' function must take a throwing function argument}}
func f3(f: UndeclaredFunctionType) rethrows { f() } // expected-error {{use of undeclared type 'UndeclaredFunctionType'}}

func cf1(f: () throws -> ())() rethrows { try f() }
func cf2(f: () -> ())() rethrows { f() } // expected-error {{'rethrows' function must take a throwing function argument}}
func cf3(f: UndeclaredFunctionType)() rethrows { f() } // expected-error {{use of undeclared type 'UndeclaredFunctionType'}}
func cf4(f: () ->())(g: () throws -> ()) rethrows {}
func cf5() rethrows -> () throws -> () {} // expected-error {{'rethrows' function must take a throwing function argument}}

/** Protocol conformance checking ********************************************/

protocol P {
  func tf() throws
  func nf() // expected-note {{protocol requires}}

  func thf(f: () throws -> ()) throws
  func nhf(f: () throws -> ()) // expected-note 2 {{protocol requires}}
  func rhf(f: () throws -> ()) rethrows // expected-note {{protocol requires}}
}

struct T0 : P { // expected-error {{type 'T0' does not conform to protocol 'P'}}
  func tf() throws {}
  func nf() throws {} // expected-note {{candidate throws, but protocol does not allow it}}

  func thf(f: () throws -> ()) throws {}
  func nhf(f: () throws -> ()) throws {} // expected-note {{candidate throws, but protocol does not allow it}}
  func rhf(f: () throws -> ()) throws {} // expected-note {{candidate is not 'rethrows', but protocol requires it}}
}

struct T1 : P {
  func tf() {}
  func nf() {}

  func thf(f: () throws -> ()) {}
  func nhf(f: () throws -> ()) {}
  func rhf(f: () throws -> ()) {}
}

struct T2 : P { // expected-error {{type 'T2' does not conform to protocol 'P'}}
  func tf() {}
  func nf() {}

  func thf(f: () throws -> ()) rethrows {}
  func nhf(f: () throws -> ()) rethrows {} // expected-note {{candidate throws, but protocol does not allow it}}
  func rhf(f: () throws -> ()) rethrows {}
}

/** Classes ******************************************************************/

class Super {
  func tf() throws {}
  func nf() {} // expected-note {{overridden declaration is here}}

  func thf(f: () throws -> ()) throws {}
  func nhf(f: () throws -> ()) {} // expected-note 2 {{overridden declaration is here}}
  func rhf(f: () throws -> ()) rethrows {} // expected-note {{overridden declaration is here}}
}

class C1 : Super {
  override func tf() {}
  override func nf() {}

  override func thf(f: () throws -> ()) {}
  override func nhf(f: () throws -> ()) {}
  override func rhf(f: () throws -> ()) {}
}

class C2 : Super {
  override func tf() throws {}
  override func nf() throws {} // expected-error {{cannot override non-throwing method with throwing method}}

  override func thf(f: () throws -> ()) throws {}
  override func nhf(f: () throws -> ()) throws {} // expected-error {{cannot override non-throwing method with throwing method}}
  override func rhf(f: () throws -> ()) throws {} // expected-error {{override of 'rethrows' method should also be 'rethrows'}}
}

class C3 : Super {
  override func tf() {}
  override func nf() {}

  override func thf(f: () throws -> ()) rethrows {}
  override func nhf(f: () throws -> ()) rethrows {} // expected-error {{cannot override non-throwing method with throwing method}}
  override func rhf(f: () throws -> ()) rethrows {}
}

/** Semantics ****************************************************************/

func call(fn: () throws -> Int) rethrows -> Int { return try fn() }
func callAC(@autoclosure fn: () throws -> Int) rethrows -> Int { return try fn() }
func raise() throws -> Int { return 0 }
func noraise() -> Int { return 0 }

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

func testForward1(fn: () throws -> Int) rethrows {
  try call(fn)
}
func testForward2(fn: () throws -> Int) rethrows {
  try call({ try fn() })
}
