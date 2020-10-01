// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -import-objc-header %S/Inputs/objc_init_override_kind.h %s

// REQUIRES: objc_interop

import Foundation

// rdar://problem/56674158
class Derived : Base {
  // This is not flagged as an error, because Base.init() is a
  // convenience init.
  init() { super.init(foo: 123) }

  required init?(coder: NSCoder) {}
}

