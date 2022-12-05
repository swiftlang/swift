// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol_helper.swift -enable-library-evolution -disable-availability-checking
// RUN: %target-swift-frontend -emit-irgen %s -I %t -module-name test | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

// rdar://102246128
// REQUIRES: PTRSIZE=64

@_weakLinked import has_symbol_helper

public func testGlobalFunctions() {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper8function4withySi_tFTwS"()
  if #_hasSymbol(function(with:)) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper12throwingFuncyyKFTwS"()
  if #_hasSymbol(throwingFunc) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper11genericFuncyyxAA1PRzlFTwS"()
  if #_hasSymbol(genericFunc(_:) as (S) -> Void) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper20funcWithOpaqueResultQryFTwS"()
  if #_hasSymbol(funcWithOpaqueResult) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper9cdeclFuncyyFTwS"()
  if #_hasSymbol(cdeclFunc) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper19forwardDeclaredFuncyyFTwS"()
  if #_hasSymbol(forwardDeclaredFunc) {}

  // FIXME: Test `dynamic` functions
  // FIXME: Test `dynamic` functions with opaque return types
}

// --- function(with:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper8function4withySi_tFTwS"()
// CHECK:   ret i1 icmp ne (void ({{i32|i64}})* @"$s17has_symbol_helper8function4withySi_tF", void (i64)* null)

// --- throwingFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper12throwingFuncyyKFTwS"()
// CHECK:   ret i1 icmp ne (void (%swift.refcounted*, %swift.error**)* @"$s17has_symbol_helper12throwingFuncyyKF", void (%swift.refcounted*, %swift.error**)* null)

// --- genericFunc(_:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper11genericFuncyyxAA1PRzlFTwS"()
// CHECK:   ret i1 icmp ne (void (%swift.opaque*, %swift.type*, i8**)* @"$s17has_symbol_helper11genericFuncyyxAA1PRzlF", void (%swift.opaque*, %swift.type*, i8**)* null)

// --- funcWithOpaqueResult() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper20funcWithOpaqueResultQryFTwS"()
// CHECK:   ret i1 and (i1 icmp ne (%swift.type_descriptor* @"$s17has_symbol_helper20funcWithOpaqueResultQryFQOMQ", %swift.type_descriptor* null), i1 icmp ne (void (%swift.opaque*)* @"$s17has_symbol_helper20funcWithOpaqueResultQryF", void (%swift.opaque*)* null))

// --- cdeclFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper9cdeclFuncyyFTwS"()
// CHECK:   ret i1 and (i1 icmp ne (void ()* @"$s17has_symbol_helper9cdeclFuncyyF", void ()* null), i1 icmp ne (void ()* @cdecl_func, void ()* null))

// --- forwardDeclaredFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper19forwardDeclaredFuncyyFTwS"()
// CHECK:   ret i1 icmp ne (void ()* @forward_declared_func, void ()* null)

public func testVars() {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper6globalSivpTwS"()
  if #_hasSymbol(global) {}
}

// --- global ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper6globalSivpTwS"()
// CHECK:   ret i1 icmp ne (i64 ()* @"$s17has_symbol_helper6globalSivg", i64 ()* null)

public func testClass(_ c: C) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1CCACycfcTwS"()
  if #_hasSymbol(C.init) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1CC6method4withySi_tFTwS"()
  if #_hasSymbol(c.method(with:)) {}
}

// --- C.init() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1CCACycfcTwS"()
// CHECK:   ret i1 and (i1 icmp ne (%T17has_symbol_helper1CC* (%T17has_symbol_helper1CC*)* @"$s17has_symbol_helper1CCACycfc", %T17has_symbol_helper1CC* (%T17has_symbol_helper1CC*)* null), i1 icmp ne (%T17has_symbol_helper1CC* (%swift.type*)* @"$s17has_symbol_helper1CCACycfC", %T17has_symbol_helper1CC* (%swift.type*)* null))

// --- C.method(with:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1CC6method4withySi_tFTwS"()
// CHECK:   ret i1 and (i1 icmp ne (void (i64, %T17has_symbol_helper1CC*)* @"$s17has_symbol_helper1CC6method4withySi_tFTj", void (i64, %T17has_symbol_helper1CC*)* null), i1 icmp ne (%swift.method_descriptor* @"$s17has_symbol_helper1CC6method4withySi_tFTq", %swift.method_descriptor* null))

public func testStruct(_ s: S) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1SV6memberSivpTwS"()
  if #_hasSymbol(s.member) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1SV6method4withySi_tFTwS"()
  if #_hasSymbol(s.method(with:)) {}
}

// --- S.member ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1SV6memberSivpTwS"()
// CHECK:   ret i1 and (i1 and (i1 and (i1 icmp ne (%swift.type_descriptor* @"$s17has_symbol_helper1SV6memberSivpMV", %swift.type_descriptor* null), i1 icmp ne (i64 (%swift.opaque*)* @"$s17has_symbol_helper1SV6memberSivg", i64 (%swift.opaque*)* null)), i1 icmp ne (void (i64, %swift.opaque*)* @"$s17has_symbol_helper1SV6memberSivs", void (i64, %swift.opaque*)* null)), i1 icmp ne ({ i8*, %TSi* } (i8*, %swift.opaque*)* @"$s17has_symbol_helper1SV6memberSivM", { i8*, %TSi* } (i8*, %swift.opaque*)* null))

// --- S.method(with:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1SV6method4withySi_tFTwS"()
// CHECK:   ret i1 icmp ne (void (i64, %swift.opaque*)* @"$s17has_symbol_helper1SV6method4withySi_tF", void (i64, %swift.opaque*)* null)

public func testEnum(_ e: E) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1EO9basicCaseyA2CmFTwS"()
  if #_hasSymbol(E.basicCase) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS"()
  if #_hasSymbol(E.payloadCase) {}
}

// --- E.basicCase ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1EO9basicCaseyA2CmFTwS"()
// CHECK:   ret i1 icmp ne (i32* @"$s17has_symbol_helper1EO9basicCaseyA2CmFWC", i32* null)

// --- E.payloadCase(_:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS"()
// CHECK:   ret i1 icmp ne (i32* @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFWC", i32* null)

public func testOpaqueParameter<T: P>(_ p: T) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP11requirementyyFTwS"()
  if #_hasSymbol(p.requirement) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyFTwS"()
  if #_hasSymbol(p.requirementWithDefaultImpl) {}
}

// --- P.requirement() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1PP11requirementyyFTwS"()
// CHECK:   ret i1 icmp ne (void (%swift.opaque*, %swift.type*, i8**)* @"$s17has_symbol_helper1PP11requirementyyF", void (%swift.opaque*, %swift.type*, i8**)* null)

// --- P.requirementWithDefaultImpl() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyFTwS"()
// CHECK:   ret i1 icmp ne (void (%swift.opaque*, %swift.type*, i8**)* @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyF", void (%swift.opaque*, %swift.type*, i8**)* null)

public func testExistentialParameter(_ p: any P) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP11requirementyyFTwS"()
  if #_hasSymbol(p.requirement) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyFTwS"()
  if #_hasSymbol(p.requirementWithDefaultImpl) {}
}

public func testMetatypes() {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1SVTwS"()
  if #_hasSymbol(S.self) {}
}

// --- S.self ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1SVTwS"()
// CHECK:   ret i1 and (i1 and (i1 icmp ne (%swift.type_descriptor* @"$s17has_symbol_helper1SVMn", %swift.type_descriptor* null), i1 icmp ne (%swift.type* @"$s17has_symbol_helper1SVN", %swift.type* null)), i1 icmp ne (%swift.metadata_response (i64)* @"$s17has_symbol_helper1SVMa", %swift.metadata_response (i64)* null))
