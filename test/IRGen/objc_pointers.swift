// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %swift -sdk %S/Inputs -I %t -primary-file %s -emit-ir | FileCheck %s

import Foundation

@objc class Foo {
  // CHECK: define internal void @_TToFC13objc_pointers3Foo16pointerArgumentsfS0_FTGVSs20UnsafeMutablePointerSi_1yGS1_T__1zGVSs13UnsafePointerSi_1wGVSs33AutoreleasingUnsafeMutablePointerGSqS0____T_(%0*, i8*, i64*, i8*, i64*, %0**)
  @objc func pointerArguments(x: UnsafeMutablePointer<Int>,
                              y: UnsafeMutablePointer<Void>,
                              z: UnsafePointer<Int>,
                              w: AutoreleasingUnsafeMutablePointer<Foo?>) {}
}
