// RUN: %target-swift-emit-silgen -primary-file %s | %FileCheck %s

// Currently, this only appears for memberwise initializers.

struct A {
  var b: Int = 0
  var c: Bool = false
}

// CHECK: function_ref variable initialization expression of A.c
// CHECK-NEXT: [[C1_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1cSbvpfi : $@convention(thin) () -> Bool
// CHECK-NEXT: [[C1:%.*]] = apply [[C1_REF]]() : $@convention(thin) () -> Bool
// CHECK-NEXT: function_ref A.init(b:c:)
// CHECK-NEXT: [[A1_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1b1cACSi_SbtcfC : $@convention(method) (Int, Bool, @thin A.Type) -> A
// CHECK-NEXT: {{.*}} = apply [[A1_REF]]({{.*}}, [[C1]], {{.*}}) : $@convention(method) (Int, Bool, @thin A.Type) -> A

let d = A(b: 1)

// CHECK: function_ref variable initialization expression of A.b
// CHECK-NEXT: [[B1_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1bSivpfi : $@convention(thin) () -> Int
// CHECK-NEXT: [[B1:%.*]] = apply [[B1_REF]]() : $@convention(thin) () -> Int
// CHECK-NEXT: function_ref A.init(b:c:)
// CHECK-NEXT: [[A2_REF:%.*]] = function_ref @$s27stored_property_default_arg1AV1b1cACSi_SbtcfC : $@convention(method) (Int, Bool, @thin A.Type) -> A
// CHECK-NEXT: {{.*}} = apply [[A2_REF]]([[B1]], {{.*}}, {{.*}}) : $@convention(method) (Int, Bool, @thin A.Type) -> A

let e = A(c: true)

struct F<T> {
  var g: T
  var h: Int = 0
}

// CHECK: function_ref variable initialization expression of F.h
// CHECK-NEXT: [[H1_REF:%.*]] = function_ref @$s27stored_property_default_arg1FV1hSivpfi : $@convention(thin) <τ_0_0> () -> Int
// CHECK-NEXT: [[H1:%.*]] = apply [[H1_REF]]<Int>() : $@convention(thin) <τ_0_0> () -> Int
// CHECK-NEXT: function_ref F.init(g:h:)
// CHECK-NEXT: [[F1_REF:%.*]] = function_ref @$s27stored_property_default_arg1FV1g1hACyxGx_SitcfC : $@convention(method) <τ_0_0> (@in τ_0_0, Int, @thin F<τ_0_0>.Type) -> @out F<τ_0_0>
// CHECK-NEXT: {{.*}} = apply [[F1_REF]]<Int>({{.*}}, {{.*}}, [[H1]], {{.*}}) : $@convention(method) <τ_0_0> (@in τ_0_0, Int, @thin F<τ_0_0>.Type) -> @out F<τ_0_0>

let i = F(g: 128)
