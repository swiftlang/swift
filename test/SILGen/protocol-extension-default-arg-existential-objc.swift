// RUN: %target-swift-frontend -emit-sil -verify %s -swift-version 3
// REQUIRES: objc_interop

import Foundation

class Foo: NSObject {
  dynamic func bridged(x: String = "foo") {}
}

func invokeMethodsWithDefaultArgs(x: Foo) {
  x.bridged()
}
