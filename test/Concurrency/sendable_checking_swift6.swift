// RUN: %target-swift-frontend -emit-sil -swift-version 6 %s -o /dev/null -verify

// REQUIRES: concurrency


struct UnavailableSendable {}

@available(*, unavailable)
extension UnavailableSendable: Sendable {}
// expected-note@-1 {{conformance of 'UnavailableSendable' to 'Sendable' has been explicitly marked unavailable here}}

@available(SwiftStdlib 5.1, *)
func checkOpaqueType() -> some Sendable {
  UnavailableSendable()
  // expected-error@-1 {{conformance of 'UnavailableSendable' to 'Sendable' is unavailable}}
}
