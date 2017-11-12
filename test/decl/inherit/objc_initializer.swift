// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %s -verify
// REQUIRES: objc_interop
import Foundation

class Foo : NSObject {
  @objc(initWithInt:)
  init(value: Int) { }

  @objc(initWithString:)
  init(value: String) { }
}

class Bar : Foo { // okay: 
}
