// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency

// Redeclaration checking
func redecl1() async { } // expected-note{{previously declared here}}
func redecl1() async throws { } // expected-error{{invalid redeclaration of 'redecl1()'}}

func redecl2() -> String { "" } // okay
func redecl2() async -> String { "" } // okay

// Override checking

class Super {
  func f() async { } // expected-note{{potential overridden instance method 'f()' here}}
  func g() { } // expected-note{{potential overridden instance method 'g()' here}}
  func h() async { }
}

class Sub: Super {
  override func f() { } // expected-error{{method does not override any method from its superclass}}
  override func g() async { } // expected-error{{method does not override any method from its superclass}}
  override func h() async { }
}

// Witness checking
protocol P1 {
  func g() // expected-note{{protocol requires function 'g()' with type '() -> ()'}}
}

struct ConformsToP1: P1 { // expected-error{{type 'ConformsToP1' does not conform to protocol 'P1'}} expected-note {{add stubs for conformance}}
  func g() async { }  // expected-note{{candidate is 'async', but protocol requirement is not}}
}

protocol P2 {
  func f() async
}

struct ConformsToP2: P2 {
  func f() { }  // okay
}

// withoutActuallyEscaping on async functions
func takeEscaping(_: @escaping () async -> Void) async { }

func thereIsNoEscape(_ body: () async -> Void) async {
  await withoutActuallyEscaping(body) { escapingBody in
    await takeEscaping(escapingBody)
  }
}

func testAsyncExistentialOpen(_ v: P1) async {
  func syncUnderlyingType<T>(u: T) {}
  func syncThrowsUnderlyingType<T>(u: T) throws {}

  func asyncUnderlyingType<T>(u: T) async {}
  func asyncThrowsUnderlyingType<T>(u: T) async throws {}

  _openExistential(v, do: syncUnderlyingType)
  await _openExistential(v, do: asyncUnderlyingType)

  try! _openExistential(v, do: syncThrowsUnderlyingType)
  try! await _openExistential(v, do: asyncThrowsUnderlyingType)
}
