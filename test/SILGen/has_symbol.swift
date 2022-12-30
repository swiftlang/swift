// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/has_symbol_helper.swiftmodule -parse-as-library %S/Inputs/has_symbol_helper.swift -enable-library-evolution
// RUN: %target-swift-frontend -emit-silgen %s -I %t -module-name test | %FileCheck %s

// UNSUPPORTED: OS=windows-msvc

@_weakLinked import has_symbol_helper

// CHECK: sil hidden [ossa] @$s4test0A15GlobalFunctionsyyF : $@convention(thin) () -> ()
func testGlobalFunctions() {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #function
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(function(with:)) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #genericFunc
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(genericFunc(_:) as (S) -> Void) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #cdeclFunc
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(cdeclFunc) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #forwardDeclaredFunc
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(forwardDeclaredFunc) {}
}

// --- function(with:) ---
// CHECK: sil @$s17has_symbol_helper8function4withySi_tF : $@convention(thin) (Int) -> ()

// --- genericFunc<A>(_:) ---
// CHECK: sil @$s17has_symbol_helper11genericFuncyyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()

// --- cdeclFunc() ---
// CHECK: sil @$s17has_symbol_helper9cdeclFuncyyF : $@convention(thin) () -> ()
// CHECK: sil [serialized] @cdecl_func : $@convention(c) () -> ()

// --- forwardDeclaredFunc() ---
// CHECK: sil @forward_declared_func : $@convention(thin) () -> ()


// CHECK: sil hidden [ossa] @$s4test0A4VarsyyF : $@convention(thin) () -> ()
func testVars() {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #global
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(global) {}
}

// --- global ---
// CHECK: sil @$s17has_symbol_helper6globalSivg : $@convention(thin) () -> Int


// CHECK: sil hidden [ossa] @$s4test0A6Structyy17has_symbol_helper1SVF : $@convention(thin) (@in_guaranteed S) -> ()
func testStruct(_ s: S) {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #S.member
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.member) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #S.method
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.method(with:)) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #S.genericFunc
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.genericFunc(_:) as (S) -> ()) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #S.staticFunc
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.staticFunc) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #S.staticMember
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.staticMember) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #S.init
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.init(member:)) {}
}

// --- S.member ---
// CHECK: sil @$s17has_symbol_helper1SV6memberSivg : $@convention(method) (@in_guaranteed S) -> Int
// CHECK: sil @$s17has_symbol_helper1SV6memberSivs : $@convention(method) (Int, @inout S) -> ()
// CHECK: sil @$s17has_symbol_helper1SV6memberSivM : $@yield_once @convention(method) (@inout S) -> @yields @inout Int

// --- S.method(with:) ---
// CHECK: sil @$s17has_symbol_helper1SV6method4withySi_tF : $@convention(method) (Int, @in_guaranteed S) -> ()

// --- S.genericFunc<A>(_:) ---
// CHECK: sil @$s17has_symbol_helper1SV11genericFuncyyxAA1PRzlF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @in_guaranteed S) -> ()

// --- S.staticFunc() ---
// CHECK: sil @$s17has_symbol_helper1SV10staticFuncyyFZ : $@convention(method) (@thin S.Type) -> ()

// --- S.staticMember ---
// CHECK: sil @$s17has_symbol_helper1SV12staticMemberSivgZ : $@convention(method) (@thin S.Type) -> Int
// CHECK: sil @$s17has_symbol_helper1SV12staticMemberSivsZ : $@convention(method) (Int, @thin S.Type) -> ()
// CHECK: sil @$s17has_symbol_helper1SV12staticMemberSivMZ : $@yield_once @convention(method) (@thin S.Type) -> @yields @inout Int

// --- S.init(member:) ---
// sil @$s17has_symbol_helper1SV6memberACSi_tcfC : $@convention(method) (Int, @thin S.Type) -> @out S


// CHECK: sil hidden [ossa] @$s4test0A13GenericStructyy17has_symbol_helper0B1SVyAC1SVGF : $@convention(thin) (@in_guaranteed GenericS<S>) -> ()
func testGenericStruct(_ s: GenericS<S>) {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #GenericS.member
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.member) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #GenericS.method
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(s.method(with:)) {}
}

// --- GenericS.member ---
// CHECK: sil @$s17has_symbol_helper8GenericSV6memberxvg : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed GenericS<τ_0_0>) -> @out τ_0_0
// CHECK: sil @$s17has_symbol_helper8GenericSV6memberxvs : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in τ_0_0, @inout GenericS<τ_0_0>) -> ()
// CHECK: sil @$s17has_symbol_helper8GenericSV6memberxvM : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : P> (@inout GenericS<τ_0_0>) -> @yields @inout τ_0_0

// --- GenericS.method(with:) ---
// CHECK: sil @$s17has_symbol_helper8GenericSV6method4withyx_tF : $@convention(method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0, @in_guaranteed GenericS<τ_0_0>) -> ()


// CHECK: sil hidden [ossa] @$s4test0A5Classyy17has_symbol_helper1CCF : $@convention(thin) (@guaranteed C) -> () {
func testClass(_ c: C) {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #C.member
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(c.member) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #C.method
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(c.method(with:)) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #C.classFunc
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.classFunc) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #C.staticMember
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.staticMember) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #C.init
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.init(member:)) {}
}

// --- C.member ---
// Method dispatch thunks are generated in IRGen so no SIL decls are expected.

// --- C.method(with:) ---
// Method dispatch thunks are generated in IRGen so no SIL decls are expected.

// --- C.classFunc() ---
// Method dispatch thunks are generated in IRGen so no SIL decls are expected.

// --- C.staticMember ---
// CHECK: sil @$s17has_symbol_helper1CC12staticMemberSivgZ : $@convention(method) (@thick C.Type) -> Int
// CHECK: sil @$s17has_symbol_helper1CC12staticMemberSivsZ : $@convention(method) (Int, @thick C.Type) -> ()
// CHECK: sil @$s17has_symbol_helper1CC12staticMemberSivMZ : $@yield_once @convention(method) (@thick C.Type) -> @yields @inout Int

// --- C.init(member:) ---
// CHECK: sil @$s17has_symbol_helper1CC6memberACSi_tcfc : $@convention(method) (Int, @owned C) -> @owned C
// CHECK: sil [serialized] @$s17has_symbol_helper1CC6memberACSi_tcfC : $@convention(method) (Int, @thick C.Type) -> @owned C


// CHECK: sil hidden [ossa] @$s4test0A4Enumyy17has_symbol_helper1EOF : $@convention(thin) (@in_guaranteed E) -> ()
func testEnum(_ e: E) {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #E.basicCase
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(E.basicCase) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #E.payloadCase
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(E.payloadCase(_:)) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #E.method
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(e.method) {}
}

// --- E.basicCase(_:) ---

// --- E.payloadCase(_:) ---

// --- E.method() ---
// CHECK: sil @$s17has_symbol_helper1EO6methodyyF : $@convention(method) (@in_guaranteed E) -> ()


// CHECK: sil hidden [ossa] @$s4test0A15OpaqueParameteryyx17has_symbol_helper1PRzlF : $@convention(thin) <T where T : P> (@in_guaranteed T) -> ()
func testOpaqueParameter<T: P>(_ p: T) {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #P.requirement
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(p.requirement) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #P.requirementWithDefaultImpl
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(p.requirementWithDefaultImpl) {}
}

// --- P.requirement() ---
// CHECK: sil @$s17has_symbol_helper1PP11requirementyyF : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()

// --- P.requirementWithDefaultImpl() ---
// CHECK: sil @$s17has_symbol_helper1PP26requirementWithDefaultImplyyF : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> ()

// CHECK: sil hidden [ossa] @$s4test0A20ExistentialParameteryy17has_symbol_helper1P_pF : $@convention(thin) (@in_guaranteed any P) -> ()
func testExistentialParameter(_ p: any P) {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #P.requirement
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(p.requirement) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #P.requirementWithDefaultImpl
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(p.requirementWithDefaultImpl) {}
}

// CHECK: sil hidden [ossa] @$s4test0A9MetatypesyyF : $@convention(thin) () -> ()
func testMetatypes() {
  // CHECK: [[RES:%[0-9]+]] = has_symbol #P
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(P.self) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #S
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(S.self) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #GenericS
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(GenericS<S>.self) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #C
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(C.self) {}
  // CHECK: [[RES:%[0-9]+]] = has_symbol #E
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(E.self) {}
}

// --- P.self ---

// --- S.self ---

// --- GenericS.self ---

// --- C.self ---

// --- E.self ---

// CHECK: sil hidden [ossa] @$s4test0A15NotWeaklyLinkedyyF : $@convention(thin) () -> ()
func testNotWeaklyLinked() {
  // `Swift.Int` is not weakly imported so this check is a no-op.

  // CHECK: [[RES:%[0-9]+]] = integer_literal $Builtin.Int1, -1
  // CHECK: cond_br [[RES]], bb{{[0-9]+}}, bb{{[0-9]+}}
  if #_hasSymbol(Swift.Int.self) {}
}

// We should not generate a #_hasSymbol query helper for Swift.Int.
// CHECK-NOT: sSiTwS
