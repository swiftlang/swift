// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t  %S/Inputs/def_lifetime_dependence_enums.swift 

// RUN: llvm-bcanalyzer %t/def_lifetime_dependence_enums.swiftmodule 

// RUN: %target-swift-frontend -c -module-name lifetime-dependence-enums -Xllvm -sil-print-after=EarlyPerfInliner -O -I %t %s \
// RUN: -enable-experimental-feature Lifetimes -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_Lifetimes

import def_lifetime_dependence_enums 

@inline(__always)
public func testSome<T : OptionalType>(_ t: T, _ w: T.Wrapped) -> T {
  return T.self.some(w)
}

@inline(__always)
public func testSomePair<T : OptionalType>(_ t: T, _ w: T.Wrapped) -> T {
  return T.self.somePair(w, w)
}

// CHECK-LABEL: sil [ossa] @$s4main5test1yQr29def_lifetime_dependence_enums12FakeOptionalOyxG_xtlF : 
// Match the second instance of EarlyPerfInliner
// CHECK-LABEL: sil [ossa] @$s4main5test1yQr29def_lifetime_dependence_enums12FakeOptionalOyxG_xtlF : 
// CHECK: [[FUNC:%.*]] = function_ref @$s29def_lifetime_dependence_enums12FakeOptionalO4someyACyxGxcAEmlF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable, τ_0_0 : ~Escapable> (@in τ_0_0, @thin FakeOptional<τ_0_0>.Type) -> @lifetime(copy 0) @out FakeOptional<τ_0_0>
// CHECK: apply [[FUNC]]<W>({{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable, τ_0_0 : ~Escapable> (@in τ_0_0, @thin FakeOptional<τ_0_0>.Type) -> @lifetime(copy 0) @out FakeOptional<τ_0_0>
// CHECK-LABEL: } // end sil function '$s4main5test1yQr29def_lifetime_dependence_enums12FakeOptionalOyxG_xtlF'
public func test1<W>(_ f: FakeOptional<W>, _ w: FakeOptional<W>.Wrapped) -> some OptionalType {
  return testSome(f, w)
}

// CHECK-LABEL: sil [ossa] @$s4main5test2yQr29def_lifetime_dependence_enums12FakeOptionalOyxG_xtlF : 
// Match the second instance of EarlyPerfInliner
// CHECK-LABEL: sil [ossa] @$s4main5test2yQr29def_lifetime_dependence_enums12FakeOptionalOyxG_xtlF : 
// CHECK:   [[FUNC:%.*]] = function_ref @$s29def_lifetime_dependence_enums12FakeOptionalO8somePairyACyxGx_xtcAEmlF : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable, τ_0_0 : ~Escapable> (@in τ_0_0, @in τ_0_0, @thin FakeOptional<τ_0_0>.Type) -> @lifetime(copy 0, copy 1) @out FakeOptional<τ_0_0> // user: %13
// CHECK:   apply [[FUNC]]<W>({{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable, τ_0_0 : ~Escapable> (@in τ_0_0, @in τ_0_0, @thin FakeOptional<τ_0_0>.Type) -> @lifetime(copy 0, copy 1) @out FakeOptional<τ_0_0>
// CHECK-LABEL: } // end sil function '$s4main5test2yQr29def_lifetime_dependence_enums12FakeOptionalOyxG_xtlF'
public func test2<W>(_ f: FakeOptional<W>, _ w: FakeOptional<W>.Wrapped) -> some OptionalType {
  return testSomePair(f, w)
}

