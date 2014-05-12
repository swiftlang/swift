// RUN: %swift -emit-silgen %s | FileCheck %s

class B { }
class D : B { }

// CHECK-LABEL: sil  @_TF5casts6upcast
func upcast(d: D) -> B {
  // CHECK: {{%.*}} = upcast
  return d
}
// CHECK-LABEL: sil  @_TF5casts8downcast
func downcast(b: B) -> D {
  // CHECK: {{%.*}} = unconditional_checked_cast downcast
  return (b as D)!
}

// CHECK-LABEL: sil  @_TF5casts3isa
func isa(b: B) -> Bool {
  // CHECK: checked_cast_br downcast {{%.*}}, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  // CHECK: [[YES]]({{%.*}}):
  // CHECK:   integer_literal {{.*}} -1
  // CHECK: [[NO]]:
  // CHECK:   integer_literal {{.*}} 0
  return b is D
}

// CHECK-LABEL: sil  @_TF5casts16upcast_archetype
func upcast_archetype<T : B>(t: T) -> B {
  // CHECK: {{%.*}} = upcast
  return t
}

// CHECK-LABEL: sil @_TF5casts25upcast_archetype_metatype
func upcast_archetype_metatype<T : B>(t: T.Type) -> B.Type {
  // CHECK: {{%.*}} = upcast
  return t
}

// CHECK-LABEL: sil  @_TF5casts18downcast_archetype
func downcast_archetype<T : B>(b: B) -> T {
  // CHECK: {{%.*}} = unconditional_checked_cast super_to_archetype
  return (b as T)!
}

// CHECK-LABEL: sil  @_TF5casts12is_archetype
func is_archetype<T : B>(b: B) -> Bool {
  // CHECK: checked_cast_br super_to_archetype {{%.*}}, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  // CHECK: [[YES]]({{%.*}}):
  // CHECK:   integer_literal {{.*}} -1
  // CHECK: [[NO]]:
  // CHECK:   integer_literal {{.*}} 0
  return b is T
}

// CHECK: sil @_TF5casts20downcast_conditional
// CHECK:   checked_cast_br downcast {{%.*}} : $B to $D
// CHECK:   load {{.*}} : $*Optional<D>
func downcast_conditional(b: B) -> D? {
  return b as D
}

protocol P {}
struct S : P {}

// CHECK: sil @_TF5casts32downcast_existential_conditional
// CHECK:   checked_cast_br existential_to_concrete {{%.*}} : $*P to $*S
// -- The result needs to be copied out of the existential container
// CHECK: {{bb.*}}({{%.*}} : $*S):
// CHECK;   copy_addr
// CHECK;   load
// CHECK: {{bb.*}}:
// CHECK:   load {{.*}} : $*Optional<S>
func downcast_existential_conditional(p: P) -> S? {
  return p as S
}

