// RUN: not %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck
// REQUIRES: objc_interop

import Foundation

_ = Dictionary[String: Any]()

func test(obj: AnyObject) {
  obj[String: Any] // Dynamic subscript
}

#colorLiteral(String: Any)
