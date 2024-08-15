// RUN: %target-swift-frontend -emit-sil -swift-version 6 -disable-availability-checking -verify %s -o /dev/null -parse-as-library

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
}

func useValue<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

@MainActor func testGlobalFakeInit() {
  let ns = NonSendableKlass()

  // Will be resolved once @MainActor is @Sendable
  Task.fakeInit { @MainActor in
    // expected-error @-1 {{sending value of non-Sendable type '@MainActor @Sendable () async -> ()' risks causing data races}}
    // expected-note @-2 {{Passing main actor-isolated value of non-Sendable type '@MainActor @Sendable () async -> ()' as a 'sending' parameter to static method 'fakeInit(operation:)' risks causing races inbetween main actor-isolated uses and uses reachable from 'fakeInit(operation:)'}}
    print(ns)
  }

  useValue(ns)
}
