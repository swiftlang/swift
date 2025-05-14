// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modules)
// RUN: split-file %s %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/lib.swift \
// RUN:   -emit-module-path %t/modules/lib.swiftmodule -module-name lib \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -enable-experimental-feature CustomAvailability

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %t/client.swift \
// RUN:   -typecheck -verify \
// RUN:   -I %S/../Inputs/custom-modules/availability-domains \
// RUN:   -I %t/modules -enable-experimental-feature CustomAvailability

// REQUIRES: swift_feature_CustomAvailability

// https://github.com/swiftlang/swift/issues/80058
// UNSUPPORTED: OS=linux-android

//--- lib.swift

import Oceans // re-exports Rivers

@available(Pacific)
public func availableInPacific() { }

@available(Colorado, unavailable)
public func unavailableInColorado() { }

//--- client.swift

import lib

func test() {
  availableInPacific() // expected-error {{'availableInPacific()' is only available in Pacific}}
  unavailableInColorado() // expected-error {{'unavailableInColorado()' is unavailable}}
}
