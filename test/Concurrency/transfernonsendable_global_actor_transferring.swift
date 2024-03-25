// RUN: %target-swift-frontend -emit-sil -swift-version 6 -disable-availability-checking -enable-experimental-feature TransferringArgsAndResults -verify %s -o /dev/null -parse-as-library

// README: Once we loosen the parser so that transferring is rejected in Sema
// instead of the parser, move into the normal
// transfernonsendable_global_actor.swift

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

extension Task where Failure == Never {
  public static func fakeInit(
    @_implicitSelfCapture operation: transferring @escaping () async -> Success
  ) {}
}

func useValue<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

@MainActor func testGlobalFakeInit() {
  let ns = NonSendableKlass()

  // Will be resolved once @MainActor is @Sendable
  Task.fakeInit { @MainActor in // expected-error {{main actor-isolated value of type '@MainActor () async -> ()' passed as a strongly transferred parameter}}
    print(ns) // expected-error {{transferring 'ns' may cause a race}}
    // expected-note @-1 {{disconnected 'ns' is captured by a main actor-isolated closure. main actor-isolated uses in closure may race against later nonisolated uses}}
  }

  useValue(ns) // expected-note {{use here could race}}
}
