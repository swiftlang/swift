// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enforce-exclusivity=checked -swift-version 4 -emit-sil -primary-file %s -o /dev/null -verify
// REQUIRES: objc_interop

import Foundation

class SomeClass {
  @objc
  func testCallNoEscapeBlockDirectly(_ c: @convention(block) () -> ()) {
    c()
  }
}
