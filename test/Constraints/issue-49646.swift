// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s
// REQUIRES: objc_interop

// https://github.com/apple/swift/issues/49646

import Foundation

class C : NSObject, NSWobbling {
  func wobble() {}
  func returnMyself() -> Self { return self }
}

func testDynamicOptionalRequirement(_ a: AnyObject) {
  a.optionalRequirement?()
}
