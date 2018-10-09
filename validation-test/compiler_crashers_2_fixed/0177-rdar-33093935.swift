// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -primary-file %s %S/Inputs/0177-rdar-33093935-other.swift -verify

// REQUIRES: objc_interop

import Foundation

extension A {
  static func superclass() -> AnyObject? { return nil }
  @objc var name: String { return "hi" }
}

class B: A {
  @objc var foo = superclass()?.name // expected-error{{property 'foo' references itself}}
}
