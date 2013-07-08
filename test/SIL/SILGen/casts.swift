// RUN: %swift -emit-sil %s | FileCheck %s

class B { }
class D : B { }

// CHECK: sil @_T5casts6upcastFT1dCS_1D_CS_1B
func upcast(d:D) -> B {
  // CHECK: {{%.*}} = upcast
  return d as B
}
// CHECK: sil @_T5casts8downcastFT1bCS_1B_CS_1D
func downcast(b:B) -> D {
  // CHECK: {{%.*}} = downcast unconditional
  return b as! D
}
// CHECK: sil @_T5casts3isaFT1bCS_1B_Sb
func isa(b:B) -> Bool {
  // CHECK: [[CAST:%.*]] = downcast conditional
  // CHECK-NEXT: is_nonnull [[CAST]]
  return b is D
}

// CHECK: sil @_T5casts16upcast_archetypeU__FT1tQ__CS_1B
func upcast_archetype<T:B>(t:T) -> B {
  // CHECK: {{%.*}} = archetype_ref_to_super
  return t as B
}

// CHECK: sil @_T5casts18downcast_archetypeU__FT1bCS_1B_Q_
func downcast_archetype<T:B>(b:B) -> T {
  // CHECK: {{%.*}} = super_to_archetype_ref unconditional
  return b as! T
}

// CHECK: sil @_T5casts12is_archetypeU__FT1bCS_1B_Sb
func is_archetype<T:B>(b:B) -> Bool {
  // CHECK: [[CAST:%.*]] = super_to_archetype_ref conditional
  // CHECK-NEXT: is_nonnull [[CAST]]
  return b is T
}
