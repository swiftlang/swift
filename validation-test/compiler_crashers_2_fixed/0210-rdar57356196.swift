// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) %s -typecheck -verify
// REQUIRES: objc_interop

import Foundation

@objc class A : NSObject {
  @objc var x: Int = 42
}

@propertyWrapper
struct Attr<V> {
  var wrappedValue: V {
    get { fatalError() }
  }

  init(wrappedValue: V, key: KeyPath<A, V>) {}
}

class B {
  @Attr(key: \.x) var y: Int = 0 // Ok
}
