// RUN: %target-swift-frontend -emit-sil -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// REQUIRES: objc_interop

import ObjectiveC
import Foundation
import gizmo

@objc class Foo : NSObject {
  @objc(firstProp) var fooProp: Foo?
  @objc(secondProp) var stringProp: String?
}

// CHECK-LABEL: sil hidden @_T012objc_keypath13createKeyPathSSyF
func createKeyPath() -> String {
  // CHECK: string_literal utf8 "firstProp.secondProp"
  return #keyPath(Foo.fooProp.stringProp)
} // CHECK: } // end sil function '_T012objc_keypath13createKeyPathSSyF'

// CHECK-LABEL: sil hidden @_T012objc_keypath21createKeyPathImportedSSyF
func createKeyPathImported() -> String {
  // CHECK: string_literal utf8 "originalName"
  return #keyPath(Gizmo.renamedProp)
} // CHECK: } // end sil function '_T012objc_keypath21createKeyPathImportedSSyF'
