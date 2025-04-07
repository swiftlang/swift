// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -enable-upcoming-feature DeprecateApplicationMain -verify -verify-additional-prefix deprecated- %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_DeprecateApplicationMain

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' class must conform to the 'NSApplicationDelegate' protocol}}
// expected-deprecated-warning@-1 {{'NSApplicationMain' is deprecated; this is an error in the Swift 6 language mode}}
// expected-deprecated-note@-2 {{use '@main' instead}} {{1-19=@main}}
class MyNonDelegate {
}
