// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -parse-as-library -verify %s

// REQUIRES: objc_interop

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate1: NSObject, NSApplicationDelegate {
}

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate2: NSObject, NSApplicationDelegate {
}

@NSApplicationMain // expected-error{{'NSApplicationMain' attribute can only apply to one class in a module}}
class MyDelegate3: NSObject, NSApplicationDelegate {
}
