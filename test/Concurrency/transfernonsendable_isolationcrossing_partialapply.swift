// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-experimental-feature RegionBasedIsolation -disable-availability-checking -verify %s -o /dev/null

// REQUIRES: concurrency
// REQUIRES: asserts

// This test validates how we handle partial applies that are isolated to a
// specific isolation domain (causing isolation crossings to occur).

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

actor Custom {
  var x = NonSendableKlass()
}

@globalActor
struct CustomActor {
    static var shared: Custom {
        return Custom()
    }
}

func useValue<T>(_ t: T) {}
@MainActor func transferToMain<T>(_ t: T) {}

/////////////////
// MARK: Tests //
/////////////////

actor ProtectsNonSendable {
  var ns: NonSendableKlass = .init()

  nonisolated func testParameter(_ ns: NonSendableKlass) async {
    self.assumeIsolated { isolatedSelf in
      // expected-warning @-1 {{call site passes `self` or a non-sendable argument of this function to another thread, potentially yielding a race with the caller}}
      isolatedSelf.ns = ns
    }
  }

  nonisolated func testLocal() async {
    let l = NonSendableKlass()

    // This is safe since we do not reuse l.
    self.assumeIsolated { isolatedSelf in
      isolatedSelf.ns = l
    }
  }

  nonisolated func testLocal2() async {
    let l = NonSendableKlass()

    // This is not safe since we use l later.
    self.assumeIsolated { isolatedSelf in
      isolatedSelf.ns = l // expected-warning {{actor-isolated closure captures value of non-Sendable type 'NonSendableKlass' from nonisolated context; later accesses to value could race}}
    }

    useValue(l) // expected-note {{access here could race}}
  }
}

func normalFunc_testLocal_1() {
  let x = NonSendableKlass()
  let _ = { @MainActor in
    print(x)
  }
}

func normalFunc_testLocal_2() {
  let x = NonSendableKlass()
  let _ = { @MainActor in
    useValue(x) // expected-warning {{main actor-isolated closure captures value of non-Sendable type 'NonSendableKlass' from nonisolated context; later accesses to value could race}}
  }
  useValue(x) // expected-note {{access here could race}}
}
