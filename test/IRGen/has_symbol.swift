// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol_helper.swift -enable-library-evolution -disable-availability-checking
// RUN: %target-swift-frontend -emit-irgen %s -I %t -module-name test | %FileCheck %s

// REQUIRES: VENDOR=apple

@_weakLinked import has_symbol_helper

func testGlobalFunctions() {
  if #_hasSymbol(function(with:)) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper8function4withySi_tFTwS"()
  // CHECK:   ret i1 icmp ne (void (i64)* @"$s17has_symbol_helper8function4withySi_tF", void (i64)* null)

  if #_hasSymbol(throwingFunc) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper12throwingFuncyyKFTwS"()
  // CHECK:   ret i1 icmp ne (void (%swift.refcounted*, %swift.error**)* @"$s17has_symbol_helper12throwingFuncyyKF", void (%swift.refcounted*, %swift.error**)* null)

  if #_hasSymbol(genericFunc(_:) as (S) -> Void) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper11genericFuncyyxAA1PRzlFTwS"()
  // CHECK:   ret i1 icmp ne (void (%swift.opaque*, %swift.type*, i8**)* @"$s17has_symbol_helper11genericFuncyyxAA1PRzlF", void (%swift.opaque*, %swift.type*, i8**)* null)

  if #_hasSymbol(funcWithOpaqueResult) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper20funcWithOpaqueResultQryFTwS"()
  // CHECK:   ret i1 and (i1 icmp ne (%swift.type_descriptor* @"$s17has_symbol_helper20funcWithOpaqueResultQryFQOMQ", %swift.type_descriptor* null), i1 icmp ne (void (%swift.opaque*)* @"$s17has_symbol_helper20funcWithOpaqueResultQryF", void (%swift.opaque*)* null))

  if #_hasSymbol(cdeclFunc) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper9cdeclFuncyyFTwS"()
  // CHECK:   ret i1 and (i1 icmp ne (void ()* @"$s17has_symbol_helper9cdeclFuncyyF", void ()* null), i1 icmp ne (void ()* @cdecl_func, void ()* null))

  if #_hasSymbol(forwardDeclaredFunc) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper19forwardDeclaredFuncyyFTwS"()
  // CHECK:   ret i1 icmp ne (void ()* @forward_declared_func, void ()* null)
  
  // FIXME: Test `dynamic` functions
  // FIXME: Test `dynamic` functions with opaque return types
}

func testVars() {
  if #_hasSymbol(global) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper6globalSivpTwS"()
  // CHECK:   ret i1 icmp ne (i64 ()* @"$s17has_symbol_helper6globalSivg", i64 ()* null)
}

func testClass(_ c: C) {
  if #_hasSymbol(C.init) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1CCACycfcTwS"()
  // CHECK:   ret i1 and (i1 icmp ne (%T17has_symbol_helper1CC* (%T17has_symbol_helper1CC*)* @"$s17has_symbol_helper1CCACycfc", %T17has_symbol_helper1CC* (%T17has_symbol_helper1CC*)* null), i1 icmp ne (%T17has_symbol_helper1CC* (%swift.type*)* @"$s17has_symbol_helper1CCACycfC", %T17has_symbol_helper1CC* (%swift.type*)* null))

  if #_hasSymbol(c.method(with:)) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1CC6method4withySi_tFTwS"()
  // CHECK:   ret i1 and (i1 icmp ne (void (i64, %T17has_symbol_helper1CC*)* @"$s17has_symbol_helper1CC6method4withySi_tFTj", void (i64, %T17has_symbol_helper1CC*)* null), i1 icmp ne (%swift.method_descriptor* @"$s17has_symbol_helper1CC6method4withySi_tFTq", %swift.method_descriptor* null))
}

func testStruct(_ s: S) {
  if #_hasSymbol(s.member) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1SV6memberSivpTwS"()
  // CHECK:   ret i1 and (i1 and (i1 and (i1 icmp ne (%swift.type_descriptor* @"$s17has_symbol_helper1SV6memberSivpMV", %swift.type_descriptor* null), i1 icmp ne (i64 (%swift.opaque*)* @"$s17has_symbol_helper1SV6memberSivg", i64 (%swift.opaque*)* null)), i1 icmp ne (void (i64, %swift.opaque*)* @"$s17has_symbol_helper1SV6memberSivs", void (i64, %swift.opaque*)* null)), i1 icmp ne ({ i8*, %TSi* } (i8*, %swift.opaque*)* @"$s17has_symbol_helper1SV6memberSivM", { i8*, %TSi* } (i8*, %swift.opaque*)* null))

  if #_hasSymbol(s.method(with:)) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1SV6method4withySi_tFTwS"()
  // CHECK:   ret i1 icmp ne (void (i64, %swift.opaque*)* @"$s17has_symbol_helper1SV6method4withySi_tF", void (i64, %swift.opaque*)* null)
}

func testEnum(_ e: E) {
  if #_hasSymbol(E.basicCase) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1EO9basicCaseyA2CmFTwS"()
  // CHECK:   ret i1 icmp ne (i32* @"$s17has_symbol_helper1EO9basicCaseyA2CmFWC", i32* null)

  if #_hasSymbol(E.payloadCase) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS"()
  // CHECK:   ret i1 icmp ne (i32* @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFWC", i32* null)
}

func testMetatypes() {
  if #_hasSymbol(S.self) {}
  // CHECK: define linkonce_odr hidden swiftcc i1 @"$s17has_symbol_helper1SVTwS"()
  // CHECK:   ret i1 and (i1 and (i1 icmp ne (%swift.type_descriptor* @"$s17has_symbol_helper1SVMn", %swift.type_descriptor* null), i1 icmp ne (%swift.type* @"$s17has_symbol_helper1SVN", %swift.type* null)), i1 icmp ne (%swift.metadata_response (i64)* @"$s17has_symbol_helper1SVMa", %swift.metadata_response (i64)* null))
}
