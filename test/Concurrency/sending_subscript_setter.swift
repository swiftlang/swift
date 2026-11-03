// RUN: %target-swift-frontend -swift-version 6 -emit-sil -verify %s -o /dev/null

// REQUIRES: concurrency

class NS: ~Sendable {}

private func assertIsDisconnected<T>(_: sending T) {}

struct S {
    let ns = NS()

    subscript(key: String) -> sending NS {
        get {
            return ns // expected-error {{sending 'self.ns' risks causing data races}}
            // expected-note @-1 {{'self.ns' cannot be a 'sending' result. Code in the current task may race with caller uses}}
        }
        set {
            assertIsDisconnected(newValue)
        }
    }
}
