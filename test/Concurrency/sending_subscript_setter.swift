// RUN: %target-swift-frontend -swift-version 6 %s -emit-silgen | %FileCheck %s
// RUN: %target-swift-frontend -swift-version 6 %s -emit-sil -verify -o /dev/null

// REQUIRES: concurrency

class NS: ~Sendable {}

private func consumeDisconnected<T>(_: sending T) {}

// MARK: - get + set

// CHECK-LABEL: sil hidden [ossa] @$s24sending_subscript_setter12GetterSetterV{{.*}}ig :
// CHECK-SAME: -> @sil_sending @owned NS
struct GetterSetter {
    subscript(key: String) -> sending NS {
        get { NS() }
        // CHECK-LABEL: sil hidden [ossa] @$s24sending_subscript_setter12GetterSetterV{{.*}}is :
        // CHECK-SAME: (@sil_sending @owned NS,
        set {
            consumeDisconnected(newValue)
        }
    }
}

// MARK: - Getter returning non-disconnected value

struct GetterError {
    let ns = NS()
    subscript(key: String) -> sending NS {
        get {
            return ns // expected-error {{sending 'self.ns' risks causing data races}}
            // expected-note @-1 {{'self.ns' cannot be a 'sending' result. Code in the current task may race with caller uses}}
        }
        set {
            consumeDisconnected(newValue)
        }
    }
}
