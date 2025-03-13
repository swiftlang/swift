// RUN: %target-swift-frontend -typecheck -verify -import-objc-header %S/Inputs/availability_domains_bridging_header.h -enable-experimental-feature CustomAvailability %s

// REQUIRES: swift_feature_CustomAvailability

@available(BayBridge)
func availableInBayBridge() {}

@available(BayBridge, unavailable)
func unavailableInBayBridge() {} // expected-note {{'unavailableInBayBridge()' has been explicitly marked unavailable here}}

func testSwiftDecls() {
  availableInBayBridge() // expected-error {{'availableInBayBridge()' is only available in BayBridge}}
  unavailableInBayBridge() // expected-error {{'unavailableInBayBridge()' is unavailable}}
}
