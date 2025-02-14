// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 4 -O -Xllvm -sil-disable-pass=compute-side-effects -Xllvm -sil-disable-pass=cmo -whole-module-optimization -emit-module-path %t/LibCore.swiftmodule %S/Inputs/xref-private-type/LibCore.swift
// RUN: %target-build-swift -swift-version 4 -O -Xllvm -sil-disable-pass=compute-side-effects -Xllvm -sil-disable-pass=cmo -I %t  -whole-module-optimization -emit-module-path %t/Lib.swiftmodule %S/Inputs/xref-private-type/Lib.swift 
// RUN: %target-build-swift -swift-version 4 -O -Xllvm -sil-disable-pass=copy-to-borrow-optimization -I %t -Xllvm -sil-print-types -emit-sil %s | %FileCheck %s

import Lib

// CHECK: sil{{.*}} @[[TEST1:[^ ]+5test1[^ ]+]] :
func test1() {
  // The important lines in this test are the strong_retains, which refer to
  // private types defined in LibCore. Normally we shouldn't have references to
  // non-public declarations in inlinable code, but because SIL passes can break
  // apart non-resilient structs and enums we can end up in that situation.
  // Worse, this can happen *across module boundaries.*
  //
  // In this test, the addressor for each global defined in Lib ends up
  // referencing private types defined in LibCore. Using those globals in
  // default argument position leads to the addressor getting inlined into
  // calling code in Swift 4 and later. This results in an attempt to not just
  // reference a private type, but to *resolve a cross-reference to a private
  // type.*
  //
  // This is the situation in https://github.com/apple/swift/issues/49423
  // (simplified). I'm not sure of a simpler way to reliably trigger the issue.
  // But if this test breaks, please try to find one.
  //
  // (We may want to revisit this whole thing later, as it violates the model.
  // But it's also useful for performance in non-resilient code.)

  // CHECK: [[ADDR:%.+]] = global_addr @{{[^ ]+}}3Lib7lazyFoo
  // CHECK: [[LOADED:%.+]] = load [[ADDR]] : $*Foo
  // CHECK: [[REF:%.+]] = struct_extract [[LOADED]] : $Foo, #Foo.ref
  // CHECK: strong_retain [[REF]] : $TopLevelInternalClass
  // CHECK: apply {{%.+}}([[LOADED]])
  testFoo()

  // CHECK: [[ADDR:%.+]] = global_addr @{{[^ ]+}}3Lib7lazyBar
  // CHECK: [[LOADED:%.+]] = load [[ADDR]] : $*Bar
  // CHECK: [[REF:%.+]] = struct_extract [[LOADED]] : $Bar, #Bar.ref
  // CHECK: strong_retain [[REF]] : $Bar.NestedInternalClass
  // CHECK: apply {{%.+}}([[LOADED]])
  testBar()

  // CHECK: [[ADDR:%.+]] = global_addr @{{[^ ]+}}3Lib7lazyBaz
  // CHECK: [[LOADED:%.+]] = load [[ADDR]] : $*Baz
  // CHECK: [[REF:%.+]] = struct_extract [[LOADED]] : $Baz, #Baz.ref
  // CHECK: strong_retain [[REF]] : $Baz.NestedInternalClass.DoublyNestedInternalClass
  // CHECK: apply {{%.+}}([[LOADED]])
  testBaz()
} // CHECK: end sil function '[[TEST1]]'

