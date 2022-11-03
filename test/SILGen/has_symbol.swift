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

// --- function(with:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper8function4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper8function4withySi_tF : $@convention(thin) (Int) -> ()

// --- genericFunc<A>(_:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper11genericFuncyyxAA1PRzlFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper11genericFuncyyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()

// --- cdeclFunc() ---
// CHECK: sil hidden_external @$s17has_symbol_helper9cdeclFuncyyFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper9cdeclFuncyyF : $@convention(thin) () -> ()
// CHECK: sil [serialized] @cdecl_func : $@convention(c) () -> ()

// --- forwardDeclaredFunc() ---
// CHECK: sil hidden_external @$s17has_symbol_helper19forwardDeclaredFuncyyFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @forward_declared_func : $@convention(thin) () -> ()


// CHECK: sil hidden [ossa] @$s4test0A4VarsyyF : $@convention(thin) () -> ()
func testVars() {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper6globalSivpTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(global) {}
}

// --- global ---
// CHECK: sil hidden_external @$s17has_symbol_helper6globalSivpTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper6globalSivg : $@convention(thin) () -> Int


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

// --- S.member ---
// CHECK: sil hidden_external @$s17has_symbol_helper1SV6memberSivpTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1SV6memberSivg : $@convention(method) (@in_guaranteed S) -> Int
// CHECK: sil @$s17has_symbol_helper1SV6memberSivs : $@convention(method) (Int, @inout S) -> ()
// CHECK: sil @$s17has_symbol_helper1SV6memberSivM : $@yield_once @convention(method) (@inout S) -> @yields @inout Int

// --- S.method(with:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper1SV6method4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1SV6method4withySi_tF : $@convention(method) (Int, @in_guaranteed S) -> ()

// --- S.genericFunc<A>(_:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper1SV11genericFuncyyxAA1PRzlFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1SV11genericFuncyyxAA1PRzlF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @in_guaranteed S) -> ()

// --- S.staticFunc() ---
// CHECK: sil hidden_external @$s17has_symbol_helper1SV10staticFuncyyFZTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1SV10staticFuncyyFZ : $@convention(method) (@thin S.Type) -> ()

// --- S.staticMember ---
// CHECK: sil hidden_external @$s17has_symbol_helper1SV12staticMemberSivpZTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1SV12staticMemberSivgZ : $@convention(method) (@thin S.Type) -> Int
// CHECK: sil @$s17has_symbol_helper1SV12staticMemberSivsZ : $@convention(method) (Int, @thin S.Type) -> ()
// CHECK: sil @$s17has_symbol_helper1SV12staticMemberSivMZ : $@yield_once @convention(method) (@thin S.Type) -> @yields @inout Int

// --- S.init(member:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper1SV6memberACSi_tcfcTwS : $@convention(thin) () -> Builtin.Int1
// sil @$s17has_symbol_helper1SV6memberACSi_tcfC : $@convention(method) (Int, @thin S.Type) -> @out S


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

// --- GenericS.member ---
// CHECK: sil hidden_external @$s17has_symbol_helper8GenericSV6memberxvpTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper8GenericSV6memberxvg : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed GenericS<τ_0_0>) -> @out τ_0_0
// CHECK: sil @$s17has_symbol_helper8GenericSV6memberxvs : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in τ_0_0, @inout GenericS<τ_0_0>) -> ()
// CHECK: sil @$s17has_symbol_helper8GenericSV6memberxvM : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : P> (@inout GenericS<τ_0_0>) -> @yields @inout τ_0_0

// --- GenericS.method(with:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper8GenericSV6method4withyx_tFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper8GenericSV6method4withyx_tF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @in_guaranteed GenericS<τ_0_0>) -> ()


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

// --- C.member ---
// CHECK: sil hidden_external @$s17has_symbol_helper1CC6memberSivpTwS : $@convention(thin) () -> Builtin.Int1
// Method dispatch thunks are generated in IRGen so no SIL decls are expected.

// --- C.method(with:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper1CC6method4withySi_tFTwS : $@convention(thin) () -> Builtin.Int1
// Method dispatch thunks are generated in IRGen so no SIL decls are expected.

// --- C.classFunc() ---
// CHECK: sil hidden_external @$s17has_symbol_helper1CC9classFuncyyFZTwS : $@convention(thin) () -> Builtin.Int1
// Method dispatch thunks are generated in IRGen so no SIL decls are expected.

// --- C.staticMember ---
// CHECK: sil hidden_external @$s17has_symbol_helper1CC12staticMemberSivpZTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1CC12staticMemberSivgZ : $@convention(method) (@thick C.Type) -> Int
// CHECK: sil @$s17has_symbol_helper1CC12staticMemberSivsZ : $@convention(method) (Int, @thick C.Type) -> ()
// CHECK: sil @$s17has_symbol_helper1CC12staticMemberSivMZ : $@yield_once @convention(method) (@thick C.Type) -> @yields @inout Int

// --- C.init(member:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper1CC6memberACSi_tcfcTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1CC6memberACSi_tcfc : $@convention(method) (Int, @owned C) -> @owned C
// CHECK: sil [serialized] @$s17has_symbol_helper1CC6memberACSi_tcfC : $@convention(method) (Int, @thick C.Type) -> @owned C


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

// --- E.basicCase(_:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper1EO9basicCaseyA2CmFTwS : $@convention(thin) () -> Builtin.Int1

// --- E.payloadCase(_:) ---
// CHECK: sil hidden_external @$s17has_symbol_helper1EO11payloadCaseyAcA1SVcACmFTwS : $@convention(thin) () -> Builtin.Int1

// --- E.method() ---
// CHECK: sil hidden_external @$s17has_symbol_helper1EO6methodyyFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1EO6methodyyF : $@convention(method) (@in_guaranteed E) -> ()


// CHECK: sil hidden [ossa] @$s4test0A8Protocolyyx17has_symbol_helper1PRzlF : $@convention(thin) <T where T : P> (@in_guaranteed T) -> ()
func testProtocol<T: P>(_ p: T) {
  // CHECK: [[QUERY:%[0-9]+]] = function_ref @$s17has_symbol_helper1PP11requirementyyFTwS : $@convention(thin) () -> Builtin.Int1
  // CHECK: [[RES:%[0-9]+]] = apply [[QUERY]]() : $@convention(thin) () -> Builtin.Int1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(p.requirement) {}
}

// --- P.requirement() ---
// CHECK: sil hidden_external @$s17has_symbol_helper1PP11requirementyyFTwS : $@convention(thin) () -> Builtin.Int1
// CHECK: sil @$s17has_symbol_helper1PP11requirementyyF : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()


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

// --- P.self ---
// CHECK: sil hidden_external @$s17has_symbol_helper1PPTwS : $@convention(thin) () -> Builtin.Int1

// --- S.self ---
// CHECK: sil hidden_external @$s17has_symbol_helper1SVTwS : $@convention(thin) () -> Builtin.Int1

// --- GenericS.self ---
// CHECK: sil hidden_external @$s17has_symbol_helper8GenericSVTwS : $@convention(thin) () -> Builtin.Int1

// --- C.self ---
// CHECK: sil hidden_external @$s17has_symbol_helper1CCTwS : $@convention(thin) () -> Builtin.Int1

// --- E.self ---
// CHECK: sil hidden_external @$s17has_symbol_helper1EOTwS : $@convention(thin) () -> Builtin.Int1

// CHECK: sil hidden [ossa] @$s4test0A15NotWeaklyLinkedyyF : $@convention(thin) () -> ()
func testNotWeaklyLinked() {
  // `Swift.Int` is not weakly imported so this check is a no-op.

  // CHECK: [[RES:%[0-9]+]] = integer_literal $Builtin.Int1, -1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(Swift.Int.self) {}
}

// We should not generate a #_hasSymbol query helper for Swift.Int.
// CHECK-NOT: sSiTwS
