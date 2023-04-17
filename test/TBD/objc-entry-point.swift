// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -verify -enable-testing %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -verify %s

// REQUIRES: objc_interop
import AppKit

// Globals in non-script mode files that still have entry points
// (via `@NSApplicationMain`) _do_ have lazy initializers. Ensure the symbols are
// present in the TBD.
let globalConstantWithLazyInitializer: String = "hello, world"

@NSApplicationMain // expected-warning {{'NSApplicationMain' is deprecated; this is an error in Swift 6}}
// expected-note@-1 {{use @main instead}} {{1-19=@main}}
class MyDelegate: NSObject, NSApplicationDelegate {
}
