// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated \
// RUN:   -import-objc-header %S/Inputs/availability_domains_bridging_header.h \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   %s %S/Inputs/availability_custom_domains_other.swift

// Re-test with the bridging header precompiled into a .pch.
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-pch \
// RUN:   -o %t/bridging-header.pch %S/Inputs/availability_domains_bridging_header.h

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify -verify-ignore-unrelated \
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
  func availableInBayBridge() { }
  // expected-error@-1 {{instance method 'availableInBayBridge()' does not match the declaration in the header because it must be only available in BayBridge}} {{3-3=@available(BayBridge)\n  }}

  func unavailableInBayBridge() { }
  // expected-error@-1 {{instance method 'unavailableInBayBridge()' does not match the declaration in the header because it must be unavailable in BayBridge}} {{3-3=@available(BayBridge, unavailable)\n  }}

  @available(GoldenGateBridge)
  func availableInGoldenGateBridge() { }

  @available(GoldenGateBridge, unavailable)
  func unavailableInGoldenGateBridge() { }
}

@objc @implementation
extension ImplementMe2 {
  // expected-error@-1 {{extension for main class interface does not provide all required implementations}}
  // expected-note@-2 {{missing instance method 'availableInBayBridge()'}}
  // expected-note@-3 {{missing instance method 'unavailableInBayBridge()'}}
  // expected-note@-4 {{add stubs for missing '@implementation' requirements}}
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
  // expected-error@-1 {{'@objc @implementation' extension cannot implement class 'ImplementMeGoldenGateBridgeAvailable2' because it is only available in BayBridge}}
  // expected-note@-2 {{add '@available' attribute to enclosing extension}}
}

// This implementation is rejected because it is less available than the
// original class declaration.
@available(BayBridge)
@available(GoldenGateBridge)
@objc @implementation
extension ImplementMeGoldenGateBridgeAvailable3 {
  // expected-error@-1 {{'@objc @implementation' extension cannot implement class 'ImplementMeGoldenGateBridgeAvailable3' because it is only available in BayBridge}}
}

@available(GoldenGateBridge, unavailable)
@objc @implementation
extension ImplementMeGoldenGateBridgeUnavailable {
}
