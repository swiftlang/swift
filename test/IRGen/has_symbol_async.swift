// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol_helper.swift -enable-library-evolution -disable-availability-checking -DCONCURRENCY
// RUN: %target-swift-frontend -emit-irgen %s -I %t -module-name test | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: VENDOR=apple

@_weakLinked import has_symbol_helper

func testGlobalFunctions() {
  if #_hasSymbol(asyncFunc) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper9asyncFuncyyYaFTwS"()
  // CHECK:   ret i1 and (i1 icmp ne (void (%swift.context*)* @"$s17has_symbol_helper9asyncFuncyyYaF", void (%swift.context*)* null), i1 icmp ne (%swift.async_func_pointer* @"$s17has_symbol_helper9asyncFuncyyYaFTu", %swift.async_func_pointer* null))

  if #_hasSymbol(isolatedFunc) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper12isolatedFuncyyFTwS"()
  // CHECK:   ret i1 icmp ne (void ()* @"$s17has_symbol_helper12isolatedFuncyyF", void ()* null)
}

func testActor(_ a: A) {
  if #_hasSymbol(A.init) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1ACACycfcTwS"()
  // CHECK:   ret i1 and (i1 icmp ne (%T17has_symbol_helper1AC* (%T17has_symbol_helper1AC*)* @"$s17has_symbol_helper1ACACycfc", %T17has_symbol_helper1AC* (%T17has_symbol_helper1AC*)* null), i1 icmp ne (%T17has_symbol_helper1AC* (%swift.type*)* @"$s17has_symbol_helper1ACACycfC", %T17has_symbol_helper1AC* (%swift.type*)* null))

  if #_hasSymbol(a.asyncMethod) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1AC11asyncMethodyyYaFTwS"()
  // CHECK:   ret i1 and (i1 and (i1 icmp ne (void (%swift.context*, %T17has_symbol_helper1AC*)* @"$s17has_symbol_helper1AC11asyncMethodyyYaFTj", void (%swift.context*, %T17has_symbol_helper1AC*)* null), i1 icmp ne (%swift.async_func_pointer* @"$s17has_symbol_helper1AC11asyncMethodyyYaFTjTu", %swift.async_func_pointer* null)), i1 icmp ne (%swift.method_descriptor* @"$s17has_symbol_helper1AC11asyncMethodyyYaFTq", %swift.method_descriptor* null))

  // FIXME: Add support for actor isolated methods
}
