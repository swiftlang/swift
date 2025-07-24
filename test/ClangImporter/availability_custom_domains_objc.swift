// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify \
// RUN:   -import-objc-header %S/Inputs/availability_domains_bridging_header.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   %s %S/Inputs/availability_custom_domains_other.swift

// Re-test with the bridging header precompiled into a .pch.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-pch \
// RUN:   -o %t/bridging-header.pch %S/Inputs/availability_domains_bridging_header.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify \
// RUN:   -import-objc-header %t/bridging-header.pch \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   %s %S/Inputs/availability_custom_domains_other.swift

// REQUIRES: swift_feature_CustomAvailability
// REQUIRES: objc_interop

import Oceans // re-exports Rivers

func testObjCClasses( // expected-note {{add '@available' attribute to enclosing global function}}
  _ bayBridgeAvailable: BayBridgeAvailable, // expected-error {{'BayBridgeAvailable' is only available in BayBridge}}
  _ bayBridgeUnavailable: BayBridgeUnavailable, // expected-error {{'BayBridgeUnavailable' is unavailable}}
) { }

@objc @implementation
extension ImplementMe {
  // FIXME: [availability] @available(BayBridge) should be required
  func availableInBayBridge() { }

  // FIXME: [availability] Should match and suggest @available(BayBridge, unavailable)
  func unavailableInBayBridge() { } // expected-error {{instance method 'unavailableInBayBridge()' does not match any instance method declared in the headers for 'ImplementMe'}}
  // expected-note@-1 {{add 'private' or 'fileprivate'}}
  // expected-note@-2 {{add 'final' to define a Swift-only instance method}}

  @available(GoldenGateBridge)
  func availableInGoldenGateBridge() { }

  // FIXME: [availability] This should be accepted
  @available(GoldenGateBridge, unavailable)
  func unavailableInGoldenGateBridge() { } // expected-error {{instance method 'unavailableInGoldenGateBridge()' does not match any instance method declared in the headers for 'ImplementMe'}}
  // expected-note@-1 {{add 'private' or 'fileprivate'}}
  // expected-note@-2 {{add 'final' to define a Swift-only instance method}}
}

@objc @implementation
extension ImplementMeBayBridgeAvailable { // expected-error {{'ImplementMeBayBridgeAvailable' is only available in BayBridge}}
  // expected-note@-1 {{add '@available' attribute to enclosing extension}}
}

@objc @implementation
extension ImplementMeBayBridgeUnavailable { // expected-error {{'ImplementMeBayBridgeUnavailable' is unavailable}}
}

@available(GoldenGateBridge)
@objc @implementation
extension ImplementMeGoldenGateBridgeAvailable {
}

@available(BayBridge)
@objc @implementation
extension ImplementMeGoldenGateBridgeAvailable2 { // expected-error {{'ImplementMeGoldenGateBridgeAvailable2' is only available in GoldenGateBridge}}
  // expected-note@-1 {{add '@available' attribute to enclosing extension}}
}

// FIXME: [availability] This implementation should be rejected because its less
// available than the original class declaration.
@available(BayBridge)
@available(GoldenGateBridge)
@objc @implementation
extension ImplementMeGoldenGateBridgeAvailable3 {
}

@available(GoldenGateBridge, unavailable)
@objc @implementation
extension ImplementMeGoldenGateBridgeUnavailable {
}
