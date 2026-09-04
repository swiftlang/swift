// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: objc_interop

import UIKit

@UIApplicationMain
class MyDelegate: NSObject, UIApplicationDelegate {
  func hi() { print(greeting) }
}

let greeting = "hello"
