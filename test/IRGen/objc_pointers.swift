// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation

@objc class Foo : NSObject {
  // CHECK: define internal void @"$s13objc_pointers3FooC16pointerArguments_1y1z1wySpySiG_SvSPySiGSAyACSgGtFTo"(%0*, i8*, i64*, i8*, i64*, %0**)
  @objc func pointerArguments(_ x: UnsafeMutablePointer<Int>,
                              y: UnsafeMutableRawPointer,
                              z: UnsafePointer<Int>,
                              w: AutoreleasingUnsafeMutablePointer<Foo?>) {}

  // CHECK: define internal void @"$s13objc_pointers3FooC24pointerMetatypeArguments1x1yySAyyXlXpG_SAyyXlXpSgGtFTo"(%0*, i8*, i8**, i8**)
  @objc func pointerMetatypeArguments(x: AutoreleasingUnsafeMutablePointer<AnyClass>,
                                      y: AutoreleasingUnsafeMutablePointer<AnyClass?>) {}
}

// CHECK-LABEL: s13objc_pointers14returnNSObject3objSo0D0CAE_tF
func returnNSObject(obj: NSObject) -> NSObject {
  // CHECK-NOT: return
  // CHECK: @objc_retain
  return obj
}
