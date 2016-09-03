// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import ObjectiveC
import Foundation

@objc class Foo : NSObject {
  @objc(firstProp) var fooProp: Foo?
  @objc(secondProp) var stringProp: String?
}

// CHECK-LABEL: sil hidden @_TF12objc_keypath13createKeyPathFT_SS
func createKeyPath() -> String {
  // CHECK: string_literal utf8 "firstProp.secondProp"
  return #keyPath(Foo.fooProp.stringProp)
}
