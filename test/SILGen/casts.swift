// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

class B { }
class D : B { }

// CHECK-LABEL: sil hidden @_TF5casts6upcast
func upcast(d: D) -> B {
  // CHECK: {{%.*}} = upcast
  return d
}
// CHECK-LABEL: sil hidden @_TF5casts8downcast
func downcast(b: B) -> D {
  // CHECK: {{%.*}} = unconditional_checked_cast
  return b as! D
}

// CHECK-LABEL: sil hidden @_TF5casts3isa
func isa(b: B) -> Bool {
  // CHECK: checked_cast_br {{%.*}}, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  // CHECK: [[YES]]({{%.*}}):
  // CHECK:   integer_literal {{.*}} -1
  // CHECK: [[NO]]:
  // CHECK:   integer_literal {{.*}} 0
  return b is D
}

// CHECK-LABEL: sil hidden @_TF5casts16upcast_archetype
func upcast_archetype<T : B>(t: T) -> B {
  // CHECK: {{%.*}} = upcast
  return t
}

// CHECK-LABEL: sil hidden @_TF5casts25upcast_archetype_metatype
func upcast_archetype_metatype<T : B>(t: T.Type) -> B.Type {
  // CHECK: {{%.*}} = upcast
  return t
}

// CHECK-LABEL: sil hidden @_TF5casts18downcast_archetype
func downcast_archetype<T : B>(b: B) -> T {
  // CHECK: {{%.*}} = unconditional_checked_cast
  return b as! T
}

// CHECK-LABEL: sil hidden @_TF5casts12is_archetype
func is_archetype<T : B>(b: B) -> Bool {
  // CHECK: checked_cast_br {{%.*}}, [[YES:bb[0-9]+]], [[NO:bb[0-9]+]]
  // CHECK: [[YES]]({{%.*}}):
  // CHECK:   integer_literal {{.*}} -1
  // CHECK: [[NO]]:
  // CHECK:   integer_literal {{.*}} 0
  return b is T
}

// CHECK: sil hidden @_TF5casts20downcast_conditional
// CHECK:   checked_cast_br {{%.*}} : $B to $D
// CHECK:   bb{{[0-9]+}}({{.*}} : $Optional<D>)
func downcast_conditional(b: B) -> D? {
  return b as? D
}

protocol P {}
struct S : P {}

// CHECK: sil hidden @_TF5casts32downcast_existential_conditional
// CHECK: bb0([[IN:%.*]] : $*P):
// CHECK:   [[COPY:%.*]] = alloc_stack $P
// CHECK:   copy_addr [[IN]] to [initialization] [[COPY]]#1
// CHECK:   [[TMP:%.*]] = alloc_stack $S
// CHECK:   checked_cast_addr_br take_always P in [[COPY]]#1 : $*P to S in [[TMP]]#1 : $*S, bb1, bb2
//   Success block.
// CHECK: bb1:
// CHECK:   [[T0:%.*]] = load [[TMP]]#1 : $*S
// CHECK:   [[T1:%.*]] = enum $Optional<S>, #Optional.Some!enumelt.1, [[T0]] : $S
// CHECK:   dealloc_stack [[TMP]]#0
// CHECK:   dealloc_stack [[COPY]]#0
// CHECK:   br bb3([[T1]] : $Optional<S>)
//   Failure block.
// CHECK: bb2:
// CHECK:   [[T0:%.*]] = enum $Optional<S>, #Optional.None!enumelt
// CHECK:   dealloc_stack [[TMP]]#0
// CHECK:   dealloc_stack [[COPY]]#0
// CHECK:   br bb3([[T0]] : $Optional<S>)
//   Continuation block.
// CHECK: bb3([[RESULT:%.*]] : $Optional<S>):
// CHECK:   destroy_addr [[IN]] : $*P
// CHECK:   return [[RESULT]]
func downcast_existential_conditional(p: P) -> S? {
  return p as? S
}

