// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol/has_symbol_helper.swift -enable-library-evolution -disable-availability-checking -DCONCURRENCY
// RUN: %target-swift-frontend %use_no_opaque_pointers -emit-irgen %s -I %t -module-name test | %FileCheck %s
// RUN: %target-swift-frontend -emit-irgen %s -I %t -module-name test

// REQUIRES: concurrency
// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper

public func testGlobalFunctions() {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper9asyncFuncyyYaFTwS"()
  if #_hasSymbol(asyncFunc) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper12isolatedFuncyyFTwS"()
  if #_hasSymbol(isolatedFunc) {}
}

// --- asyncFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper9asyncFuncyyYaFTwS"()
// CHECK:   ret i1 and (i1 icmp ne (void (%swift.context*)* @"$s17has_symbol_helper9asyncFuncyyYaF", void (%swift.context*)* null), i1 icmp ne (%swift.async_func_pointer* @"$s17has_symbol_helper9asyncFuncyyYaFTu", %swift.async_func_pointer* null))

// --- isolatedFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper12isolatedFuncyyFTwS"()
// CHECK:   ret i1 icmp ne (void ()* @"$s17has_symbol_helper12isolatedFuncyyF", void ()* null)

public func testActor(_ a: A) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1ACACycfcTwS"()
  if #_hasSymbol(A.init) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1AC11asyncMethodyyYaFTwS"()
  if #_hasSymbol(a.asyncMethod) {}

  // FIXME: Add support for actor isolated methods
}

// --- A.init() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1ACACycfcTwS"()
// CHECK:   ret i1 and (i1 icmp ne (%T17has_symbol_helper1AC* (%T17has_symbol_helper1AC*)* @"$s17has_symbol_helper1ACACycfc", %T17has_symbol_helper1AC* (%T17has_symbol_helper1AC*)* null), i1 icmp ne (%T17has_symbol_helper1AC* (%swift.type*)* @"$s17has_symbol_helper1ACACycfC", %T17has_symbol_helper1AC* (%swift.type*)* null))

// --- A.asyncMethod() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1AC11asyncMethodyyYaFTwS"()
// CHECK:   ret i1 and (i1 and (i1 icmp ne (void (%swift.context*, %T17has_symbol_helper1AC*)* @"$s17has_symbol_helper1AC11asyncMethodyyYaFTj", void (%swift.context*, %T17has_symbol_helper1AC*)* null), i1 icmp ne (%swift.async_func_pointer* @"$s17has_symbol_helper1AC11asyncMethodyyYaFTjTu", %swift.async_func_pointer* null)), i1 icmp ne (%swift.method_descriptor* @"$s17has_symbol_helper1AC11asyncMethodyyYaFTq", %swift.method_descriptor* null))
