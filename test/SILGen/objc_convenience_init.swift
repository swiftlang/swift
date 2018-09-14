// RUN: %target-swift-emit-sil(mock-sdk: %clang-importer-sdk) -swift-version 5 -verify %s

import Foundation

class X: NSObject {
  override init() {}

  static func foo() -> Self { fatalError() }

  @objc convenience init(x: Int) {
    self = type(of: self).foo()
  }
}
