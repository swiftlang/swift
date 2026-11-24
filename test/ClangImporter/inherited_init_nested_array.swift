// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s

// REQUIRES: objc_interop

import Foundation

// Check that an ObjC subclass which inherits an init with an InlineArray from
// its ObjC superclass doesn't crash.
@available(anyAppleOS 26.0, *)
func test() {
  _ = InheritsNestedArrayInit.init(grid:)
}
