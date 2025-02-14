// RUN: %target-typecheck-verify-swift -swift-version 6 -enable-experimental-feature StrictSendableMetatypes

// REQUIRES: concurrency
// REQUIRES: swift_feature_StrictSendableMetatypes

// This test checks for typecheck-only diagnostics involving non-sendable
// metatypes.

protocol Q {
  static func g()
}

nonisolated func acceptMeta<T>(_: T.Type) { }

nonisolated func staticCallThroughMetaVal<T: Q>(_: T.Type) {
  let x = T.self // expected-error{{capture of non-sendable type 'T.Type' in an isolated closure}}
  Task.detached {
    x.g() // expected-error{{capture of non-sendable type 'T.Type' in an isolated closure}}
  }
}

nonisolated func passMetaVal<T: Q>(_: T.Type) {
  let x = T.self // expected-error{{capture of non-sendable type 'T.Type' in an isolated closure}}
  Task.detached {
    acceptMeta(x) // expected-error{{capture of non-sendable type}}
  }
}

nonisolated func staticCallThroughMeta<T: Q>(_: T.Type) {
  Task.detached {
    T.g() // expected-error{{capture of non-sendable type}}
  }
}

nonisolated func passMeta<T: Q>(_: T.Type) {
  Task.detached {
    acceptMeta(T.self) // expected-error{{capture of non-sendable type 'T.Type' in an isolated closure}}
  }
}


nonisolated func staticCallThroughMetaSendable<T: Sendable & Q>(_: T.Type) {
  Task.detached {
    T.g()
  }
}

nonisolated func passMetaSendable<T: Sendable & Q>(_: T.Type) {
  Task.detached {
    acceptMeta(T.self)
  }
}

nonisolated func passMetaSendableMeta<T: SendableMetatype & Q>(_: T.Type) {
  Task.detached {
    acceptMeta(T.self)
  }
}

nonisolated func passMetaWithSendableVal<T: Sendable & Q>(_: T.Type) {
  let x = T.self
  Task.detached {
    acceptMeta(x) // okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}

nonisolated func passMetaWithMetaSendableVal<T: SendableMetatype & Q>(_: T.Type) {
  let x = T.self
  Task.detached {
    acceptMeta(x) // okay, because T is Sendable implies T.Type: Sendable
    x.g() // okay, because T is Sendable implies T.Type: Sendable
  }
}
