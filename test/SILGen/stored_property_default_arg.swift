// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -primary-file %s | %FileCheck %s

// Currently, this only appears for memberwise initializers.

// CHECK: default argument 0 of A.init(b:c:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg1AV1b1cACSi_SbtcfcfA_ : $@convention(thin) () -> Int {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   function_ref variable initialization expression of A.b
// CHECK-NEXT:   [[A_B_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1bSivpfi : $@convention(thin) () -> Int
// CHECK-NEXT:   [[A_B:%.*]] = apply [[A_B_REF]]() : $@convention(thin) () -> Int
// CHECK-NEXT:   return [[A_B]] : $Int
// CHECK-NEXT: }

// CHECK: default argument 1 of A.init(b:c:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg1AV1b1cACSi_SbtcfcfA0_ : $@convention(thin) () -> Bool {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   function_ref variable initialization expression of A.c
// CHECK-NEXT:   [[A_C_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1cSbvpfi : $@convention(thin) () -> Bool
// CHECK-NEXT:   [[A_C:%.*]] = apply [[A_C_REF]]() : $@convention(thin) () -> Bool
// CHECK-NEXT:   return [[A_C]] : $Bool
// CHECK-NEXT: }

struct A {
  var b: Int = 0
  var c: Bool = false
}

// CHECK-LABEL: sil hidden [ossa] @$s27stored_property_default_arg17checkConcreteTypeyyF : $@convention(thin) () -> () {
func checkConcreteType() {
// CHECK: function_ref default argument 1 of A.init(b:c:)
// CHECK-NEXT: [[C1_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1b1cACSi_SbtcfcfA0_ : $@convention(thin) () -> Bool
// CHECK-NEXT: [[C1:%.*]] = apply [[C1_REF]]() : $@convention(thin) () -> Bool
// CHECK-NEXT: function_ref A.init(b:c:)
// CHECK-NEXT: [[A1_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1b1cACSi_SbtcfC : $@convention(method) (Int, Bool, @thin A.Type) -> A
// CHECK-NEXT: {{.*}} = apply [[A1_REF]]({{.*}}, [[C1]], {{.*}}) : $@convention(method) (Int, Bool, @thin A.Type) -> A
  let d = A(b: 1)

// CHECK: function_ref default argument 0 of A.init(b:c:)
// CHECK-NEXT: [[B1_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1b1cACSi_SbtcfcfA_ : $@convention(thin) () -> Int
// CHECK-NEXT: [[B1:%.*]] = apply [[B1_REF]]() : $@convention(thin) () -> Int
// CHECK-NEXT: function_ref A.init(b:c:)
// CHECK-NEXT: [[A2_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1b1cACSi_SbtcfC : $@convention(method) (Int, Bool, @thin A.Type) -> A
// CHECK-NEXT: {{.*}} = apply [[A2_REF]]([[B1]], {{.*}}, {{.*}}) : $@convention(method) (Int, Bool, @thin A.Type) -> A
  let e = A(c: true)
}

// CHECK: default argument 1 of F.init(g:h:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg1FV1g1hACyxGx_SitcfcfA0_ : $@convention(thin) <T> () -> Int {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   function_ref variable initialization expression of F.h
// CHECK-NEXT:   [[F_H_REF:%.*]] = function_ref @$s27stored_property_default_arg1FV1hSivpfi : $@convention(thin) <τ_0_0> () -> Int
// CHECK-NEXT:   [[F_H:%.*]] = apply [[F_H_REF]]<T>() : $@convention(thin) <τ_0_0> () -> Int
// CHECK-NEXT:   return [[F_H]] : $Int
// CHECK-NEXT: }

struct F<T> {
  var g: T
  var h: Int = 0
}

// CHECK-LABEL: sil hidden [ossa] @$s27stored_property_default_arg16checkGenericTypeyyF : $@convention(thin) () -> () {
func checkGenericType() {
// CHECK: function_ref default argument 1 of F.init(g:h:)
// CHECK-NEXT: [[H1_REF:%.*]] = function_ref @$s27stored_property_default_arg1FV1g1hACyxGx_SitcfcfA0_ : $@convention(thin) <τ_0_0> () -> Int
// CHECK-NEXT: [[H1:%.*]] = apply [[H1_REF]]<Int>() : $@convention(thin) <τ_0_0> () -> Int
// CHECK-NEXT: function_ref F.init(g:h:)
// CHECK-NEXT: [[F1_REF:%.*]] = function_ref @$s27stored_property_default_arg1FV1g1hACyxGx_SitcfC : $@convention(method) <τ_0_0> (@in τ_0_0, Int, @thin F<τ_0_0>.Type) -> @out F<τ_0_0>
// CHECK-NEXT: {{.*}} = apply [[F1_REF]]<Int>({{.*}}, {{.*}}, [[H1]], {{.*}}) : $@convention(method) <τ_0_0> (@in τ_0_0, Int, @thin F<τ_0_0>.Type) -> @out F<τ_0_0>
  let i = F(g: 128)
}

struct J {
  lazy var k: Bool = false
  lazy var l: Int = 0
}

// CHECK-LABEL: sil hidden [ossa] @$s27stored_property_default_arg16checkOptionalNilyyF : $@convention(thin) () -> () {
func checkOptionalNil() {
// CHECK: [[L1_REF:%.*]] = enum $Optional<Int>, #Optional.none!enumelt
// CHECK-NEXT: function_ref J.init(k:l:)
// CHECK-NEXT: [[J1_REF:%.*]] = function_ref @$s27stored_property_default_arg1JV1k1lACSbSg_SiSgtcfC : $@convention(method) (Optional<Bool>, Optional<Int>, @thin J.Type) -> J
// CHECK-NEXT: {{.*}} = apply [[J1_REF]]({{.*}}, [[L1_REF]], {{.*}}) : $@convention(method) (Optional<Bool>, Optional<Int>, @thin J.Type) -> J
  let m = J(k: true)

// CHECK: [[K1_REF:%.*]] = enum $Optional<Bool>, #Optional.none!enumelt
// CHECK: function_ref J.init(k:l:)
// CHECK-NEXT: [[J2_REF:%.*]] = function_ref @$s27stored_property_default_arg1JV1k1lACSbSg_SiSgtcfC : $@convention(method) (Optional<Bool>, Optional<Int>, @thin J.Type) -> J
// CHECK-NEXT: {{.*}} = apply [[J2_REF]]([[K1_REF]], {{.*}}, {{.*}}) : $@convention(method) (Optional<Bool>, Optional<Int>, @thin J.Type) -> J
  let n = J(l: 316)
}

// CHECK: default argument 0 of O.init(p:q:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg1OV1p1qACyxGx_x_SittcfcfA_ : $@convention(thin) <T where T : ExpressibleByIntegerLiteral> () -> @out T {
// CHECK:      bb0([[PARAM_0:.*]] : $*T):
// CHECK-NEXT:   function_ref variable initialization expression of O.p
// CHECK-NEXT:   [[O_P_REF:%.*]] = function_ref @$s27stored_property_default_arg1OV1pxvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> @out τ_0_0
// CHECK-NEXT:   [[O_P:%.*]] = apply [[O_P_REF]]<T>([[PARAM_0]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> @out τ_0_0
// CHECK-NEXT:   [[RETURN:%.*]] = tuple ()
// CHECK-NEXT:   return [[RETURN]] : $()
// CHECK-NEXT: }

// CHECK: default argument 1 of O.init(p:q:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg1OV1p1qACyxGx_x_SittcfcfA0_ : $@convention(thin) <T where T : ExpressibleByIntegerLiteral> () -> (@out T, Int) {
// CHECK:      bb0([[PARAM_0:.*]] : $*T):
// CHECK-NEXT:   function_ref variable initialization expression of O.q
// CHECK-NEXT:   [[O_Q_REF:%.*]] = function_ref @$s27stored_property_default_arg1OV1qx_Sitvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> (@out τ_0_0, Int)
// CHECK-NEXT:   [[O_Q:%.*]] = apply [[O_Q_REF]]<T>([[PARAM_0]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> (@out τ_0_0, Int)
// CHECK-NEXT:   return [[O_Q]] : $Int
// CHECK-NEXT: }

struct O<T: ExpressibleByIntegerLiteral> {
  var p: T = 0
  var q: (T, Int) = (0, 0)
}

// CHECK-LABEL: sil hidden [ossa] @$s27stored_property_default_arg13checkIndirectyyF : $@convention(thin) () -> () {
func checkIndirect() {
// CHECK: function_ref default argument 0 of O.init(p:q:)
// CHECK-NEXT: [[P1_REF:%.*]] = function_ref @$s27stored_property_default_arg1OV1p1qACyxGx_x_SittcfcfA_ : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> @out τ_0_0
// CHECK-NEXT: [[TMP1:%.*]] = alloc_stack $Int
// CHECK-NEXT: [[P1:%.*]] = apply [[P1_REF]]<Int>([[TMP1]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> @out τ_0_0
// CHECK: function_ref O.init(p:q:)
// CHECK-NEXT: [[O1_REF:%.*]] = function_ref @$s27stored_property_default_arg1OV1p1qACyxGx_x_SittcfC : $@convention(method) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> (@in τ_0_0, @in τ_0_0, Int, @thin O<τ_0_0>.Type) -> @out O<τ_0_0>
// CHECK-NEXT: [[O1:%.*]] = apply [[O1_REF]]<Int>({{.*}}, {{.*}}, {{.*}}, {{.*}}, {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> (@in τ_0_0, @in τ_0_0, Int, @thin O<τ_0_0>.Type) -> @out O<τ_0_0>
  let r = O(q: (128, 316))
  
// CHECK: function_ref default argument 1 of O.init(p:q:)
// CHECK-NEXT: [[Q1_REF:%.*]] = function_ref @$s27stored_property_default_arg1OV1p1qACyxGx_x_SittcfcfA0_ : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> (@out τ_0_0, Int)
// CHECK-NEXT: [[TMP2:%.*]] = alloc_stack $Int
// CHECK-NEXT: [[Q1:%.*]] = apply [[Q1_REF]]<Int>([[TMP2]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> () -> (@out τ_0_0, Int)
// CHECK: function_ref O.init(p:q:)
// CHECK-NEXT: [[O2_REF:%.*]] = function_ref @$s27stored_property_default_arg1OV1p1qACyxGx_x_SittcfC : $@convention(method) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> (@in τ_0_0, @in τ_0_0, Int, @thin O<τ_0_0>.Type) -> @out O<τ_0_0>
// CHECK-NEXT: [[O2:%.*]] = apply [[O2_REF]]<Int>({{.*}}, {{.*}}, {{.*}}, {{.*}}, {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : ExpressibleByIntegerLiteral> (@in τ_0_0, @in τ_0_0, Int, @thin O<τ_0_0>.Type) -> @out O<τ_0_0>
  let s = O(p: 316)
}

// CHECK: default argument 0 of U.init(v:w:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg1UV1v1wAcA1TCSg_AGtcfcfA_ : $@convention(thin) () -> @owned Optional<T> {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   function_ref variable initialization expression of U.v
// CHECK-NEXT:   [[U_V_REF:%.*]] = function_ref @$s27stored_property_default_arg1UV1vAA1TCSgvpfi : $@convention(thin) () -> @owned Optional<T>
// CHECK-NEXT:   [[U_V:%.*]] = apply [[U_V_REF]]() : $@convention(thin) () -> @owned Optional<T>
// CHECK-NEXT:   return [[U_V]] : $Optional<T>
// CHECK-NEXT: }

// CHECK: default argument 1 of U.init(v:w:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg1UV1v1wAcA1TCSg_AGtcfcfA0_ : $@convention(thin) () -> @owned T {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   function_ref variable initialization expression of U.w
// CHECK-NEXT:   [[U_W_REF:%.*]] = function_ref @$s27stored_property_default_arg1UV1wAA1TCvpfi : $@convention(thin) () -> @owned T
// CHECK-NEXT:   [[U_W:%.*]] = apply [[U_W_REF]]() : $@convention(thin) () -> @owned T
// CHECK-NEXT:   return [[U_W]] : $T
// CHECK-NEXT: }

class T {}

struct U {
  weak var v = T()
  unowned var w = T()
}

// CHECK-LABEL: sil hidden [ossa] @$s27stored_property_default_arg19checkReferenceTypesyyF : $@convention(thin) () -> () {
func checkReferenceTypes() {
// CHECK: function_ref default argument 0 of U.init(v:w:)
// CHECK-NEXT: [[V1_REF:%.*]] = function_ref @$s27stored_property_default_arg1UV1v1wAcA1TCSg_AGtcfcfA_ : $@convention(thin) () -> @owned Optional<T>
// CHECK-NEXT: [[V1:%.*]] = apply [[V1_REF]]() : $@convention(thin) () -> @owned Optional<T>
// CHECK-NEXT: function_ref U.init(v:w:)
// CHECK-NEXT: [[U1_REF:%.*]] = function_ref @$s27stored_property_default_arg1UV1v1wAcA1TCSg_AGtcfC : $@convention(method) (@owned Optional<T>, @owned T, @thin U.Type) -> @out U
// CHECK-NEXT: [[U1:%.*]] = apply [[U1_REF]]({{.*}}, [[V1]], {{.*}}, {{.*}}) : $@convention(method) (@owned Optional<T>, @owned T, @thin U.Type) -> @out U
  let x = U(w: T())

// CHECK: function_ref default argument 1 of U.init(v:w:)
// CHECK-NEXT: [[W1_REF:%.*]] = function_ref @$s27stored_property_default_arg1UV1v1wAcA1TCSg_AGtcfcfA0_ : $@convention(thin) () -> @owned T
// CHECK-NEXT: [[W1:%.*]] = apply [[W1_REF]]() : $@convention(thin) () -> @owned T
// CHECK-NEXT: function_ref U.init(v:w:)
// CHECK-NEXT: [[U2_REF:%.*]] = function_ref @$s27stored_property_default_arg1UV1v1wAcA1TCSg_AGtcfC : $@convention(method) (@owned Optional<T>, @owned T, @thin U.Type) -> @out U
// CHECK-NEXT: [[U2:%.*]] = apply [[U2_REF]]({{.*}}, {{.*}}, [[W1]], {{.*}}) : $@convention(method) (@owned Optional<T>, @owned T, @thin U.Type) -> @out U
  let y = U(v: T())
}

// CHECK: default argument 0 of AA.init(ab:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s27stored_property_default_arg2AAV2abAcA1ZCSg2ac_AG2adt_tcfcfA_ : $@convention(thin) () -> (@owned Optional<Z>, @owned Optional<Z>) {
// CHECK-NEXT: bb0:
// CHECK-NEXT:   function_ref variable initialization expression of AA.ab
// CHECK-NEXT:   [[AA_AB_REF:%.*]] = function_ref @$s27stored_property_default_arg2AAV2abAA1ZCSg2ac_AG2adtvpfi : $@convention(thin) () -> (@owned Optional<Z>, @owned Optional<Z>)
// CHECK-NEXT:   [[AA_AB:%.*]] = apply [[AA_AB_REF]]() : $@convention(thin) () -> (@owned Optional<Z>, @owned Optional<Z>)
// CHECK-NEXT:   ([[ELT0:%.*]], [[ELT1:%.*]]) = destructure_tuple [[AA_AB]] : $(Optional<Z>, Optional<Z>)
// CHECK-NEXT:   [[RETURN:%.*]] = tuple ([[ELT0]] : $Optional<Z>, [[ELT1]] : $Optional<Z>)
// CHECK-NEXT:   return [[RETURN]] : $(Optional<Z>, Optional<Z>)
// CHECK-NEXT: }

class Z {}

struct AA {
  var ab: (ac: Z?, ad: Z?) = (ac: Z(), ad: Z())
}

// CHECK-LABEL: sil hidden [ossa] @$s27stored_property_default_arg19checkReferenceTupleyyF : $@convention(thin) () -> () {
func checkReferenceTuple() {
// CHECK: function_ref default argument 0 of AA.init(ab:)
// CHECK-NEXT: [[AB1_REF:%.*]] = function_ref @$s27stored_property_default_arg2AAV2abAcA1ZCSg2ac_AG2adt_tcfcfA_ : $@convention(thin) () -> (@owned Optional<Z>, @owned Optional<Z>)
// CHECK-NEXT: [[AB1:%.*]] = apply [[AB1_REF]]() : $@convention(thin) () -> (@owned Optional<Z>, @owned Optional<Z>)
// CHECK-NEXT: ([[ELT0:%.*]], [[ELT1:%.*]]) = destructure_tuple [[AB1]] : $(Optional<Z>, Optional<Z>)
// CHECK-NEXT: function_ref AA.init(ab:)
// CHECK-NEXT: [[AA1_REF:%.*]] = function_ref @$s27stored_property_default_arg2AAV2abAcA1ZCSg2ac_AG2adt_tcfC : $@convention(method) (@owned Optional<Z>, @owned Optional<Z>, @thin AA.Type) -> @owned AA
// CHECK-NEXT: [[AA1:%.*]] = apply [[AA1_REF]]([[ELT0]], [[ELT1]], {{.*}}) : $@convention(method) (@owned Optional<Z>, @owned Optional<Z>, @thin AA.Type) -> @owned AA
  let ae = AA.init(ab:)()
}

struct OptionalGeneric<T> {
  var t: T?
  var x: Int
}

// CHECK-LABEL: sil hidden [ossa] @$s27stored_property_default_arg31checkDefaultInitGenericOptionalyyF : $@convention(thin) () -> () {
func checkDefaultInitGenericOptional() {
  let og = OptionalGeneric<Int>(x: 0)

  // CHECK: [[VALUE:%.*]] = enum $Optional<Int>, #Optional.none!enumelt
  // CHECK: [[NIL:%.*]] = alloc_stack $Optional<Int>
  // CHECK: store [[VALUE]] to [trivial] [[NIL]] : $*Optional<Int>
  // CHECK: [[FN:%.*]] =  function_ref @$s27stored_property_default_arg15OptionalGenericV1t1xACyxGxSg_SitcfC : $@convention(method) <τ_0_0> (@in Optional<τ_0_0>, Int, @thin OptionalGeneric<τ_0_0>.Type) -> @out OptionalGeneric<τ_0_0>
  // CHECK: apply [[FN]]<Int>(%0, [[NIL]], {{%.*}}, %1)
}
