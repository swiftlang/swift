// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: objc_interop

import AppKit

@NSApplicationMain
class MyDelegate: NSObject, NSApplicationDelegate {
  func hi() { print(greeting) }
}

let greeting = "hello"
