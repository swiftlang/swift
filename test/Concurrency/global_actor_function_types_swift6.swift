// RUN: %target-typecheck-verify-swift  -target %target-swift-5.1-abi-triple -language-mode 6

final class NonSendable {
}

@available(*, unavailable)
extension NonSendable: Sendable {}

actor Test {
  func testNonSendableCrossingIsolationinAsync(v: NonSendable) {
    let _: () async -> NonSendable = { @MainActor in v }
    // expected-error@-1 {{cannot convert '@MainActor @Sendable () async -> NonSendable' to '() async -> NonSendable' because crossing of an isolation boundary requires parameter and result types to conform to 'Sendable' protocol}}
    // expected-note@-2 {{type 'NonSendable' does not conform to 'Sendable' protocol}}
  }
}
