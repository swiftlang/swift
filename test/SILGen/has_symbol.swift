// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol_helper.swift -enable-library-evolution
// RUN: %target-swift-frontend -emit-silgen %s -I %t -module-name test | %FileCheck %s

// REQUIRES: VENDOR=apple

@_weakLinked import has_symbol_helper

// CHECK: sil hidden [ossa] @$s4test0A15GlobalFunctionsyyF : $@convention(thin) () -> ()
func testGlobalFunctions() {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper8function4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(function(with:)) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper11genericFuncyyxAA1PRzlFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(genericFunc(_:) as (S) -> Void) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper9cdeclFuncyyFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(cdeclFunc) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper19forwardDeclaredFuncyyFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(forwardDeclaredFunc) {}
}

// CHECK: sil hidden_external @$s17has_symbol_helper8function4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper11genericFuncyyxAA1PRzlFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper9cdeclFuncyyFTwS : $@convention(thin) () -> Builtin.Int1

// CHECK: sil hidden [ossa] @$s4test0A4VarsyyF : $@convention(thin) () -> ()
func testVars() {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper6globalSivpTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(global) {}
}

// CHECK: sil hidden_external @$s17has_symbol_helper6globalSivpTwS : $@convention(thin) () -> Builtin.Int1

// CHECK: sil hidden [ossa] @$s4test0A6Structyy17has_symbol_helper1SVF : $@convention(thin) (@in_guaranteed S) -> ()
func testStruct(_ s: S) {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1SV6memberSivpTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.member) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1SV6method4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.method(with:)) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1SV11genericFuncyyxAA1PRzlFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.genericFunc(_:) as (S) -> ()) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1SV10staticFuncyyFZTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.staticFunc) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1SV12staticMemberSivpZTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.staticMember) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1SV6memberACSi_tcfcTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.init(member:)) {}
}

// CHECK: sil hidden_external @$s17has_symbol_helper1SV6memberSivpTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1SV6method4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1SV11genericFuncyyxAA1PRzlFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1SV10staticFuncyyFZTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1SV12staticMemberSivpZTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1SV6memberACSi_tcfcTwS : $@convention(thin) () -> Builtin.Int1

// CHECK: sil hidden [ossa] @$s4test0A13GenericStructyy17has_symbol_helper0B1SVyAC1SVGF : $@convention(thin) (@in_guaranteed GenericS<S>) -> ()
func testGenericStruct(_ s: GenericS<S>) {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper8GenericSV6memberxvpTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.member) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper8GenericSV6method4withyx_tFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.method(with:)) {}
}

// CHECK: sil hidden_external @$s17has_symbol_helper8GenericSV6memberxvpTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper8GenericSV6method4withyx_tFTwS : $@convention(thin) () -> Builtin.Int1

// CHECK: sil hidden [ossa] @$s4test0A5Classyy17has_symbol_helper1CCF : $@convention(thin) (@guaranteed C) -> () {
func testClass(_ c: C) {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1CC6memberSivpTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(c.member) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1CC6method4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(c.method(with:)) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1CC9classFuncyyFZTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.classFunc) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1CC12staticMemberSivpZTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.staticMember) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1CC6memberACSi_tcfcTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.init(member:)) {}
}

// CHECK: sil hidden_external @$s17has_symbol_helper1CC6memberSivpTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1CC6method4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1CC9classFuncyyFZTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1CC12staticMemberSivpZTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1CC6memberACSi_tcfcTwS : $@convention(thin) () -> Builtin.Int1

// CHECK: sil hidden [ossa] @$s4test0A4Enumyy17has_symbol_helper1EOF : $@convention(thin) (@in_guaranteed E) -> ()
func testEnum(_ e: E) {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1EO9basicCaseyA2CmFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(E.basicCase) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(E.payloadCase(_:)) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1EO6methodyyFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(e.method) {}
}

// CHECK: sil hidden_external @$s17has_symbol_helper1EO9basicCaseyA2CmFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1EO6methodyyFTwS : $@convention(thin) () -> Builtin.Int1

// CHECK: sil hidden [ossa] @$s4test0A8Protocolyyx17has_symbol_helper1PRzlF : $@convention(thin) <T where T : P> (@in_guaranteed T) -> ()
func testProtocol<T: P>(_ p: T) {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1PP11requirementyyFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(p.requirement) {}
}

// CHECK: sil hidden [ossa] @$s4test0A9MetatypesyyF : $@convention(thin) () -> ()
func testMetatypes() {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1PPTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(P.self) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1SVTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.self) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper8GenericSVTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(GenericS<S>.self) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1CCTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.self) {}
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1EOTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(E.self) {}
}

// CHECK: sil hidden_external @$s17has_symbol_helper1PPTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1SVTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper8GenericSVTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1CCTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil hidden_external @$s17has_symbol_helper1EOTwS : $@convention(thin) () -> Builtin.Int1
