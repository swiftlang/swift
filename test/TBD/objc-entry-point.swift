// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -verify -enable-testing %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -verify %s

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -enable-upcoming-feature DeprecateApplicationMain -verify -verify-additional-prefix deprecated- -enable-testing %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-upcoming-feature DeprecateApplicationMain -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -verify -verify-additional-prefix deprecated- %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_DeprecateApplicationMain
import AppKit

// Globals in non-script mode files that still have entry points
// (via `@NSApplicationMain`) _do_ have lazy initializers. Ensure the symbols are
// present in the TBD.
let globalConstantWithLazyInitializer: String = "hello, world"

@NSApplicationMain // expected-deprecated-warning {{'NSApplicationMain' is deprecated; this is an error in the Swift 6 language mode}}
// expected-deprecated-note@-1 {{use '@main' instead}} {{1-19=@main}}
class MyDelegate: NSObject, NSApplicationDelegate {
}
