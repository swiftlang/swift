// RUN: %target-swift-frontend -emit-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

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
