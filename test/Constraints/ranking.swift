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

func genericOverload<T>(_: T) {}
func genericOverload<T>(_: T?) {}
func genericOptional<T>(_: T?) {}
func genericNoOptional<T>(_: T) {}

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
  // CHECK: function_ref @$S7ranking15genericOverloadyyxlF
  genericOverload(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$S7ranking15genericOverloadyyxSglF
  genericOverload(p.q)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  // CHECK: function_ref @$S7ranking15genericOptionalyyxSglF
  genericOptional(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$S7ranking15genericOptionalyyxSglF
  genericOptional(p.q)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.p!getter.1
  // CHECK: function_ref @$S7ranking17genericNoOptionalyyxlF
  genericNoOptional(p.p)
  // CHECK: witness_method $@opened("{{.*}}") P, #P.q!getter.1
  // CHECK: function_ref @$S7ranking17genericNoOptionalyyxlF
  genericNoOptional(p.q)

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
  // CHECK: function_ref @$S7ranking15genericOverloadyyxlF
  genericOverload(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$S7ranking15genericOverloadyyxSglF
  genericOverload(t.q)
  // CHECK: witness_method $T, #P.p!getter.1
  // CHECK: function_ref @$S7ranking15genericOptionalyyxSglF
  genericOptional(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$S7ranking15genericOptionalyyxSglF
  genericOptional(t.q)
  // CHECK: witness_method $T, #P.p!getter.1
  // CHECK: function_ref @$S7ranking17genericNoOptionalyyxlF
  genericNoOptional(t.p)
  // CHECK: witness_method $T, #P.q!getter.1
  // CHECK: function_ref @$S7ranking17genericNoOptionalyyxlF
  genericNoOptional(t.q)
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

    // CHECK: witness_method $Self, #P.p!getter.1
    // CHECK: function_ref @$S7ranking15genericOverloadyyxlF
    genericOverload(self.p)
    // CHECK: witness_method $Self, #P.q!getter.1
    // CHECK: function_ref @$S7ranking15genericOverloadyyxSglF
    genericOverload(self.q)
    // CHECK: witness_method $Self, #P.p!getter.1
    // CHECK: function_ref @$S7ranking15genericOptionalyyxSglF
    genericOptional(self.p)
    // CHECK: witness_method $Self, #P.q!getter.1
    // CHECK: function_ref @$S7ranking15genericOptionalyyxSglF
    genericOptional(self.q)
    // CHECK: witness_method $Self, #P.p!getter.1
    // CHECK: function_ref @$S7ranking17genericNoOptionalyyxlF
    genericNoOptional(self.p)
    // CHECK: witness_method $Self, #P.q!getter.1
    // CHECK: function_ref @$S7ranking17genericNoOptionalyyxlF
    genericNoOptional(self.q)
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
