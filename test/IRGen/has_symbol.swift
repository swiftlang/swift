// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol/has_symbol_helper.swift -enable-library-evolution -target %target-swift-5.1-abi-triple
// RUN: %target-swift-frontend -emit-irgen %s -I %t -I %S/Inputs/has_symbol -module-name test | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper

// Match the target word size so that we can use it throughout the test
// CHECK: %swift.type = type { [[WORD:i32|i64]] }

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

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper11dynamicFuncyyFTwS"()
  if #_hasSymbol(dynamicFunc) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper15replacementFuncyyFTwS"()
  if #_hasSymbol(replacementFunc) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFTwS"()
  if #_hasSymbol(dynamicFuncOpaqueResult) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper27replacementFuncOpaqueResultQryFTwS"()
  if #_hasSymbol(replacementFuncOpaqueResult) {}
}

// --- function(with:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper8function4withySi_tFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper8function4withySi_tF", null
// CHECK:   ret i1 [[RES]]

// --- throwingFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper12throwingFuncyyKFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper12throwingFuncyyKF", null
// CHECK:   ret i1 [[RES]]

// --- genericFunc(_:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper11genericFuncyyxAA1PRzlFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper11genericFuncyyxAA1PRzlF", null
// CHECK:   ret i1 [[RES]]

// --- funcWithOpaqueResult() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper20funcWithOpaqueResultQryFTwS"()
// CHECK:  [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper20funcWithOpaqueResultQryFQOMQ", null
// CHECK:  [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper20funcWithOpaqueResultQryF", null
// CHECK:  [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:  ret i1 [[RES]]

// --- cdeclFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper9cdeclFuncyyFTwS"()
// CHECK:  [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper9cdeclFuncyyF", null
// CHECK:  [[V1:%.*]] = icmp ne ptr @cdecl_func, null
// CHECK:  [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   ret i1 [[RES]]

// --- forwardDeclaredFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper19forwardDeclaredFuncyyFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @forward_declared_func, null
// CHECK:   ret i1 [[RES]]

// --- dynamicFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper11dynamicFuncyyFTwS"()
// CHECK:  [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper11dynamicFuncyyF", null
// CHECK:  [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper11dynamicFuncyyFTX", null
// CHECK:  [[V2:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:  [[V3:%.*]] = icmp ne ptr @"$s17has_symbol_helper11dynamicFuncyyFTx", null
// CHECK:  [[RES:%.*]] = and i1 [[V2]], [[V3]]
// CHECK:   ret i1 [[RES]]

// --- replacementFunc() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper15replacementFuncyyFTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper15replacementFuncyyF", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper15replacementFuncyyFTX", null
// CHECK:   [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   ret i1 [[RES]]

// --- dynamicFuncOpaqueResult() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFQOMQ", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFQOMg", null
// CHECK:   [[V2:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   [[V3:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFQOMh", null
// CHECK:   [[V4:%.*]] = and i1 [[V2]], [[V3]]
// CHECK:   [[V5:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFQOMg", null
// CHECK:   [[V6:%.*]] = and i1 [[V4]], [[V5]]
// CHECK:   [[V7:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFQOMg", null
// CHECK:   [[V8:%.*]] = and i1 [[V6]], [[V7]]
// CHECK:   [[V9:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryF", null
// CHECK:   [[V10:%.*]] = and i1 [[V8]], [[V9]]
// CHECK:   [[V11:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFTX", null
// CHECK:   [[V12:%.*]] = and i1 [[V10]], [[V11]]
// CHECK:   [[V13:%.*]] = icmp ne ptr @"$s17has_symbol_helper23dynamicFuncOpaqueResultQryFTx", null
// CHECK:   [[RES:%.*]] = and i1 [[V12]], [[V13]]
// CHECK:   ret i1 [[RES]]

// --- replacementFuncOpaqueResult() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper27replacementFuncOpaqueResultQryFTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper27replacementFuncOpaqueResultQryFQOMQ", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper27replacementFuncOpaqueResultQryFQOMg", null
// CHECK:   [[V2:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   [[V3:%.*]] = icmp ne ptr @"$s17has_symbol_helper27replacementFuncOpaqueResultQryFQOMg", null
// CHECK:   [[V4:%.*]] = and i1 [[V2]], [[V3]]
// CHECK:   [[V5:%.*]] = icmp ne ptr @"$s17has_symbol_helper27replacementFuncOpaqueResultQryF", null
// CHECK:   [[V6:%.*]] = and i1 [[V4]], [[V5]]
// CHECK:   [[V7:%.*]] = icmp ne ptr @"$s17has_symbol_helper27replacementFuncOpaqueResultQryFTX", null
// CHECK:   [[RES:%.*]] = and i1 [[V6]], [[V7]]
// CHECK:   ret i1 [[RES]]

public func testVars() {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper6globalSivpTwS"()
  if #_hasSymbol(global) {}
}

// --- global ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper6globalSivpTwS"()
// CHECK: [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper6globalSivg", null
// CHECK:   ret i1 [[RES]]

public func testClass(_ c: C) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1CCACycfcTwS"()
  if #_hasSymbol(C.init) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1CC6method4withySi_tFTwS"()
  if #_hasSymbol(c.method(with:)) {}
}

// --- C.init() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1CCACycfcTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper1CCACycfc", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper1CCACycfC", null
// CHECK:   [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   ret i1 [[RES]]

// --- C.method(with:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1CC6method4withySi_tFTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper1CC6method4withySi_tFTj", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper1CC6method4withySi_tFTq", null
// CHECK:   [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   ret i1 [[RES]]

public func testStruct(_ s: S) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1SV6memberSivpTwS"()
  if #_hasSymbol(s.member) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1SV12staticMemberSivpZTwS"()
  if #_hasSymbol(S.staticMember) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1SV6method4withySi_tFTwS"()
  if #_hasSymbol(s.method(with:)) {}
}

// --- S.member ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1SV6memberSivpTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV6memberSivpMV", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV6memberSivg", null
// CHECK:   [[V2:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   [[V3:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV6memberSivs", null
// CHECK:   [[V4:%.*]] = and i1 [[V2]], [[V3]]
// CHECK:   [[V5:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV6memberSivM", null
// CHECK:   [[RES:%.*]] = and i1 [[V4]], [[V5]]
// CHECK:   ret i1 [[RES]]

// --- S.staticMember ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1SV12staticMemberSivpZTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV12staticMemberSivgZ", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV12staticMemberSivsZ", null
// CHECK:   [[V2:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   [[V3:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV12staticMemberSivMZ", null
// CHECK:   [[RES:%.*]] = and i1 [[V2]], [[V3]]
// CHECK:   ret i1 [[RES]]

// --- S.method(with:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1SV6method4withySi_tFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SV6method4withySi_tF", null
// CHECK:   ret i1 [[RES]]

public func testEnum(_ e: E) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1EO9basicCaseyA2CmFTwS"()
  if #_hasSymbol(E.basicCase) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS"()
  if #_hasSymbol(E.payloadCase) {}
}

// --- E.basicCase ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1EO9basicCaseyA2CmFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper1EO9basicCaseyA2CmFWC", null
// CHECK:   ret i1 [[RES]]

// --- E.payloadCase(_:) ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFWC", null
// CHECK:   ret i1 [[RES]]

public func testOpaqueParameter<T: P>(_ p: T) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP11requirementyyFTwS"()
  if #_hasSymbol(p.requirement) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyFTwS"()
  if #_hasSymbol(p.requirementWithDefaultImpl) {}
}

// --- P.requirement() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1PP11requirementyyFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper1PP11requirementyyF", null
// CHECK:   ret i1 [[RES]]

// --- P.requirementWithDefaultImpl() ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyFTwS"()
// CHECK:   [[RES:%.*]] = icmp ne ptr @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyF", null
// CHECK:   ret i1 [[RES]]

public func testExistentialParameter(_ p: any P) {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP11requirementyyFTwS"()
  if #_hasSymbol(p.requirement) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1PP26requirementWithDefaultImplyyFTwS"()
  if #_hasSymbol(p.requirementWithDefaultImpl) {}
}

public func testMetatypes() {
  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper1SVTwS"()
  if #_hasSymbol(S.self) {}

  // CHECK: %{{[0-9]+}} = call i1 @"$s17has_symbol_helper7GenericVTwS"()
  if #_hasSymbol(Generic<Void>.self) {}
}

// --- S.self ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper1SVTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SVMn", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SVN", null
// CHECK:   [[V2:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   [[V3:%.*]] = icmp ne ptr @"$s17has_symbol_helper1SVMa", null
// CHECK:   [[RES:%.*]] = and i1 [[V2]], [[V3]]
// CHECK:   ret i1 [[RES]]

// --- Generic<Void>.self ---
// CHECK: define linkonce_odr hidden i1 @"$s17has_symbol_helper7GenericVTwS"()
// CHECK:   [[V0:%.*]] = icmp ne ptr @"$s17has_symbol_helper7GenericVMn", null
// CHECK:   [[V1:%.*]] = icmp ne ptr @"$s17has_symbol_helper7GenericVMa", null
// CHECK:   [[RES:%.*]] = and i1 [[V0]], [[V1]]
// CHECK:   ret i1 [[RES]]
