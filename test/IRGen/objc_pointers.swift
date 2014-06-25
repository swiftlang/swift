// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -target x86_64-apple-darwin10 -module-cache-path %t/clang-module-cache -sdk %S/Inputs -I=%S/Inputs -enable-source-import %s -emit-ir | FileCheck %s

import Foundation

@objc class Foo {
  // CHECK: define internal void @_TToFC13objc_pointers3Foo16pointerArgumentsfS0_FTGVSs13UnsafePointerSi_1yGS1_T__1zGVSs18ConstUnsafePointerSi_1wGVSs26AutoreleasingUnsafePointerGSqS0____T_(%0*, i8*, i64*, i8*, i64*, %0**)
  @objc func pointerArguments(x: UnsafePointer<Int>,
                              y: UnsafePointer<Void>,
                              z: ConstUnsafePointer<Int>,
                              w: AutoreleasingUnsafePointer<Foo?>) {}
}
