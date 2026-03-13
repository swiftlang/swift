// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -swift-version 6 -typecheck -parse-as-library -verify %s

// REQUIRES: objc_interop

import AppKit

@NSApplicationMain // expected-error {{'NSApplicationMain' is deprecated}}
// expected-note@-1 {{use '@main' instead}} {{1-19=@main}}
class MyDelegate: NSObject, NSApplicationDelegate {
}
