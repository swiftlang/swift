// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-irgen %s -I %t -I %S/Inputs/has_symbol -module-name test | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper_clang

// CHECK-DAG: @clangGlobalVar = extern_weak global i32
// CHECK-DAG: @clangWeakImportGlobalVar = extern_weak global i32

// Weakly linked declarations with hidden visibility must not be dso_local.
// CHECK-DAG: @clangHiddenGlobalVar = extern_weak hidden global i32

public func testClangDecls() {
  // CHECK: %{{[0-9]+}} = call i1 @"$sSo9clangFuncyys5Int32VFTwS"()
  if #_hasSymbol(clangFunc(_:)) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$sSo14clangGlobalVars5Int32VvpTwS"()
  if #_hasSymbol(clangGlobalVar) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$sSo24clangWeakImportGlobalVars5Int32VvpTwS"()
  if #_hasSymbol(clangWeakImportGlobalVar) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$sSo15clangHiddenFuncyyFTwS"()
  if #_hasSymbol(clangHiddenFunc) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$sSo20clangHiddenGlobalVars5Int32VvpTwS"()
  if #_hasSymbol(clangHiddenGlobalVar) {}
}

// --- clangFunc(_:) ---
// CHECK: define linkonce_odr hidden i1 @"$sSo9clangFuncyys5Int32VFTwS"() #1{{( comdat)?}} {
// CHECK:   [[RES:%.*]] = icmp ne ptr @clangFunc, null
// CHECK:   ret i1 [[RES]]

// --- clangGlobalVar ---
// CHECK: define linkonce_odr hidden i1 @"$sSo14clangGlobalVars5Int32VvpTwS"() #1{{( comdat)?}} {
// CHECK:   [[RES:%.*]] = icmp ne ptr @clangGlobalVar, null
// CHECK:   ret i1 [[RES]]

// --- clangWeakImportGlobalVar ---
// CHECK: define linkonce_odr hidden i1 @"$sSo24clangWeakImportGlobalVars5Int32VvpTwS"() #1{{( comdat)?}} {
// CHECK:   [[RES:%.*]] = icmp ne ptr @clangWeakImportGlobalVar, null
// CHECK:   ret i1 [[RES]]

// CHECK: declare extern_weak hidden void @clangHiddenFunc()
