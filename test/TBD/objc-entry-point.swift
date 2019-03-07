// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -verify -enable-testing %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o /dev/null -validate-tbd-against-ir=all -parse-as-library -verify %s

// REQUIRES: objc_interop
import AppKit

// Globals in non-script mode files that still have entry points
// (via NSApplicationMain) _do_ have lazy initializers. Ensure the symbols are
// present in the TBD.
let globalConstantWithLazyInitializer: String = "hello, world"

@NSApplicationMain
class MyDelegate: NSObject, NSApplicationDelegate {
}
