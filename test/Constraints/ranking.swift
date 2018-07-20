// RUN: %target-swift-emit-silgen %s -verify | %FileCheck %s

protocol P {
  var p: P { get set }
  var q: P? { get set }
  func p(_: P)
  func q(_: P)
}

struct S : P {
  var p: P
  var q: P?
  func p(_: P) {}
  func q(_: P) {}
}

class Base : P {
  var p: P
  var q: P?
  func p(_: P) {}
  func q(_: P) {}
  init(_ p: P) { self.p = p }
}

class Derived : Base {
}

func generic<T>(_: T) {}
func generic<T>(_: T?) {}
func genericOpt<T>(_: T?) {}

// CHECK-LABEL: sil hidden @$S7ranking22propertyVersusFunctionyyAA1P_p_xtAaCRzlF
func propertyVersusFunction<T : P>(_ p: P, _ t: T) {
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _ = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: P = p.p
  // CHECK: function_ref @$S7ranking1PP1pyyAaB_pFTc
  let _: (P) -> () = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: P? = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: Any = p.p
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  let _: Any? = p.p

  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  // CHECK: function_ref @$S7ranking7genericyyxlF
  generic(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$S7ranking7genericyyxSglF
  generic(p.q)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  // CHECK: function_ref @$S7ranking10genericOptyyxSglF
  genericOpt(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$S7ranking10genericOptyyxSglF
  genericOpt(p.q)

  // CHECK: witness_method $T, #P.p!getter.1
  let _ = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: P = t.p
  // CHECK: function_ref @$S7ranking1PP1pyyAaB_pFTc
  let _: (P) -> () = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: P? = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: Any = t.p
  // CHECK: witness_method $T, #P.p!getter.1
  let _: Any? = t.p

  // CHECK: witness_method $T, #P.p!getter.1
  // CHECK: function_ref @$S7ranking7genericyyxlF
  generic(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$S7ranking7genericyyxSglF
  generic(t.q)
  // CHECK: witness_method $T, #P.p!getter.1
  // CHECK: function_ref @$S7ranking10genericOptyyxSglF
  genericOpt(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$S7ranking10genericOptyyxSglF
  genericOpt(t.q)
}

extension P {
  func propertyVersusFunction() {
    // CHECK: witness_method $Self, #P.p!getter.1
    let _ = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: P = self.p
    // CHECK: function_ref @$S7ranking1PP1pyyAaB_pFTc
    let _: (P) -> () = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: P? = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: Any = self.p
    // CHECK: witness_method $Self, #P.p!getter.1
    let _: Any? = self.p
  }
}

//--------------------------------------------------------------------

func f0<T>(_ x: T) {}

// FIXME: Lookup breaks if these come after f1!
class A { 
  init() {} 
};
class B : A { 
  override init() { super.init() } 
}

func f1(_ a: A) -> A { return a }
func f1(_ b: B) -> B { return b }

func testDerived(b: B) {
  // CHECK-LABEL: sil hidden @$S7ranking11testDerived1byAA1BC_tF
  // CHECK: function_ref @$S7ranking2f1yAA1BCADF
  // CHECK: function_ref @$S7ranking2f0yyxlF
  f0(f1(b))
  // CHECK: end sil function '$S7ranking11testDerived1byAA1BC_tF'
}
