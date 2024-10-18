// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify %s -o /dev/null -parse-as-library

// README: Once we loosen the parser so that sending is rejected in Sema
// instead of the parser, move into the normal
// transfernonsendable_global_actor.swift

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

extension Task where Failure == Never {
  public static func fakeInit(
    @_implicitSelfCapture operation: sending @escaping () async -> Success
  ) {}

  // This matches the current impl
  public static func fakeInit2(
    @_implicitSelfCapture @_inheritActorContext operation: sending @escaping @isolated(any) () async -> Success
  ) {}
}

func useValue<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

@MainActor func testGlobalFakeInit() {
  let ns = NonSendableKlass()

  // Will be resolved once @MainActor is @Sendable.
  Task.fakeInit { @MainActor in // expected-error {{passing closure as a 'sending' parameter risks causing data races between main actor-isolated code and concurrent execution of the closure}}
    print(ns) // expected-note {{closure captures 'ns' which is accessible to main actor-isolated code}}
  }

  useValue(ns)
}

@MainActor func testGlobalFakeInit2() {
  let ns = NonSendableKlass()

  // We shouldn't error here.
  Task.fakeInit2 { @MainActor in
    print(ns)
  }

  useValue(ns)
}
