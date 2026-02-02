// RUN: %target-swift-ide-test(mock-sdk: %clang-importer-sdk) -enable-objc-interop -print-indexed-symbols -source-filename %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

@objc
class Foo: NSObject {
  // CHECK-DAG: constructor(internal)/Swift | init(object:)
  init(object: Any?) {}
}

extension Foo {
  // CHECK-DAG: static-property(internal)/Swift | boom
  static let boom = Foo(object: self)
}
