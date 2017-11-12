// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -parse-as-library -verify %s

// REQUIRES: objc_interop

import AppKit

@NSApplicationMain // expected-error{{'NSApplicationMain' class must conform to the 'NSApplicationDelegate' protocol}}
class MyNonDelegate {
}
