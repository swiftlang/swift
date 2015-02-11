// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -verify %s

// REQUIRES: objc_interop

import AppKit

@NSApplicationMain
class MyDelegate: NSObject, NSApplicationDelegate {
}
