// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %s

// REQUIRES: objc_interop

import AppKit

@NSApplicationMain // expected-warning {{'NSApplicationMain' is deprecated; this is an error in Swift 6}}
// expected-note@-1 {{use @main instead}} {{1-19=@main}}
class MyDelegate: NSObject, NSApplicationDelegate {
}
