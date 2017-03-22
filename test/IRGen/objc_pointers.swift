// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64
// REQUIRES: objc_interop

import Foundation

@objc class Foo : NSObject {
  // CHECK: define internal void @_T013objc_pointers3FooC16pointerArgumentsySpySiG_Sv1ySPySiG1zs33AutoreleasingUnsafeMutablePointerVyACSgG1wtFTo(%0*, i8*, i64*, i8*, i64*, %0**)
  @objc func pointerArguments(_ x: UnsafeMutablePointer<Int>,
                              y: UnsafeMutableRawPointer,
                              z: UnsafePointer<Int>,
                              w: AutoreleasingUnsafeMutablePointer<Foo?>) {}

  // CHECK: define internal void @_T013objc_pointers3FooC24pointerMetatypeArgumentsys33AutoreleasingUnsafeMutablePointerVys9AnyObject_pXpG1x_AFysAG_pXpSgG1ytFTo(%0*, i8*, i8**, i8**)
  @objc func pointerMetatypeArguments(x: AutoreleasingUnsafeMutablePointer<AnyClass>,
                                      y: AutoreleasingUnsafeMutablePointer<AnyClass?>) {}
}
