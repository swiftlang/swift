// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-irgen %s -I %t -I %S/Inputs/has_symbol -module-name test | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

// The module is not imported @_weakLinked, so only declarations that Clang
// considers weak imported are weakly linked.
import has_symbol_helper_clang

// CHECK-DAG: @clangGlobalVar = external global i32
// CHECK-DAG: @clangWeakImportGlobalVar = extern_weak global i32

public func testClangDecls() {
  // CHECK: %{{[0-9]+}} = call i1 @"$sSo24clangWeakImportGlobalVars5Int32VvpTwS"()
  if #_hasSymbol(clangWeakImportGlobalVar) {}

  // Ordinary uses of a strongly linked variable remain strong.
  _ = clangGlobalVar
}

// --- clangWeakImportGlobalVar ---
// CHECK: define linkonce_odr hidden i1 @"$sSo24clangWeakImportGlobalVars5Int32VvpTwS"() #1{{( comdat)?}} {
// CHECK:   [[RES:%.*]] = icmp ne ptr @clangWeakImportGlobalVar, null
// CHECK:   ret i1 [[RES]]
