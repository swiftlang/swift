// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-macosx10.9 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s

import Foundation

@objc class Foo {
  // CHECK: define internal void @_TToFC13objc_pointers3Foo16pointerArgumentsfS0_FTGVSs20UnsafeMutablePointerSi_1yGS1_T__1zGVSs18ConstUnsafePointerSi_1wGVSs33AutoreleasingUnsafeMutablePointerGSqS0____T_(%0*, i8*, i64*, i8*, i64*, %0**)
  @objc func pointerArguments(x: UnsafeMutablePointer<Int>,
                              y: UnsafeMutablePointer<Void>,
                              z: ConstUnsafePointer<Int>,
                              w: AutoreleasingUnsafeMutablePointer<Foo?>) {}
}
