// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -verify %s

// REQUIRES: objc_interop

import Foundation

extension NSCouldConformToIndexable: @retroactive NSIndexable {
}

extension NSCouldConformToIndexable {
  func testIndex(_ i: Int) {
    _ = objectAtIndex(i) // expected-error {{'objectAtIndex' has been renamed to 'object(at:)'}}
    _ = object(at: i)
  }
}
