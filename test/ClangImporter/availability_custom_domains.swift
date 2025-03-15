// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify \
// RUN:   -import-objc-header %S/Inputs/availability_domains_bridging_header.h \
// RUN:   -I %S/Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   %s %S/Inputs/availability_custom_domains_other.swift

// REQUIRES: swift_feature_CustomAvailability
// UNSUPPORTED: OS=windows-msvc

import Oceans // re-exports Rivers

func testClangDecls() {
  // FIXME: [availability] These references should be diagnosed as unavailable
  available_in_arctic()
  unavailable_in_pacific()
  available_in_colorado_river_delta()
  available_in_colorado()
  available_in_baltic() // expected-error {{cannot find 'available_in_baltic' in scope}}
}

@available(BayBridge)
func availableInBayBridge() { }

@available(BayBridge, unavailable)
func unavailableInBayBridge() { } // expected-note {{'unavailableInBayBridge()' has been explicitly marked unavailable here}}

@available(Pacific)
func availableInPacific() { }

@available(Colorado, unavailable)
func unavailableInColorado() { } // expected-note {{'unavailableInColorado()' has been explicitly marked unavailable here}}

// The Seas module is only imported directly by the other source file.
@available(Baltic) // expected-warning {{unrecognized platform name 'Baltic'}}
func availableInBaltic() { } // expected-note {{did you mean 'availableInBaltic'}}

func testSwiftDecls() {
  availableInBayBridge() // expected-error {{'availableInBayBridge()' is only available in BayBridge}}
  unavailableInBayBridge() // expected-error {{'unavailableInBayBridge()' is unavailable}}
  availableInArctic()
  availableInPacific() // expected-error {{'availableInPacific()' is only available in Pacific}}
  unavailableInColorado() // expected-error {{'unavailableInColorado()' is unavailable}}
  availableInBaltic()
  availableInMediterranean() // expected-error {{'availableInMediterranean()' is only available in Mediterranean}}
}
