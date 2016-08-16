// RUN: %target-swift-frontend -parse-as-library -emit-silgen -enable-guaranteed-closure-contexts %s | %FileCheck %s

func use<T>(_: T) {}

func escape(_ f: () -> ()) {}

protocol P {}
class C: P {}
struct S {}

// CHECK-LABEL: sil hidden @_TF26guaranteed_closure_context19guaranteed_capturesFT_T_
func guaranteed_captures() {
  // CHECK: [[MUTABLE_TRIVIAL_BOX:%.*]] = alloc_box $S
  var mutableTrivial = S()
  // CHECK: [[MUTABLE_RETAINABLE_BOX:%.*]] = alloc_box $C
  var mutableRetainable = C()
  // CHECK: [[MUTABLE_ADDRESS_ONLY_BOX:%.*]] = alloc_box $P
  var mutableAddressOnly: P = C()

  // CHECK: [[IMMUTABLE_TRIVIAL:%.*]] = apply {{.*}} -> S
  let immutableTrivial = S()
  // CHECK: [[IMMUTABLE_RETAINABLE:%.*]] = apply {{.*}} -> @owned C
  let immutableRetainable = C()
  // CHECK: [[IMMUTABLE_ADDRESS_ONLY:%.*]] = alloc_stack $P
  let immutableAddressOnly: P = C()

  func captureEverything() {
    use((mutableTrivial, mutableRetainable, mutableAddressOnly,
         immutableTrivial, immutableRetainable, immutableAddressOnly))
  }

  // CHECK-NOT: strong_retain [[MUTABLE_TRIVIAL_BOX]]
  // CHECK-NOT: strong_retain [[MUTABLE_RETAINABLE_BOX]]
  // CHECK-NOT: strong_retain [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK-NOT: strong_retain [[IMMUTABLE_RETAINABLE]]
  // CHECK:     [[IMMUTABLE_AO_BOX:%.*]] = alloc_box $P

  // CHECK: [[FN:%.*]] = function_ref [[FN_NAME:@_TFF26guaranteed_closure_context19guaranteed_capturesFT_T_L_17captureEverythingfT_T_]]
  // CHECK: apply [[FN]]([[MUTABLE_TRIVIAL_BOX]], [[MUTABLE_RETAINABLE_BOX]], [[MUTABLE_ADDRESS_ONLY_BOX]], [[IMMUTABLE_TRIVIAL]], [[IMMUTABLE_RETAINABLE]], [[IMMUTABLE_AO_BOX]])
  captureEverything()

  // CHECK: strong_release [[IMMUTABLE_AO_BOX]]

  // CHECK-NOT: strong_retain [[MUTABLE_TRIVIAL_BOX]]
  // CHECK-NOT: strong_retain [[MUTABLE_RETAINABLE_BOX]]
  // CHECK-NOT: strong_retain [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK-NOT: strong_retain [[IMMUTABLE_RETAINABLE]]

  // -- partial_apply still takes ownership of its arguments.
  // CHECK: [[FN:%.*]] = function_ref [[FN_NAME]]
  // CHECK: strong_retain [[MUTABLE_TRIVIAL_BOX]]
  // CHECK: strong_retain [[MUTABLE_RETAINABLE_BOX]]
  // CHECK: strong_retain [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK: strong_retain [[IMMUTABLE_RETAINABLE]]
  // CHECK: [[IMMUTABLE_AO_BOX:%.*]] = alloc_box $P
  // CHECK: [[CLOSURE:%.*]] = partial_apply {{.*}}([[MUTABLE_TRIVIAL_BOX]], [[MUTABLE_RETAINABLE_BOX]], [[MUTABLE_ADDRESS_ONLY_BOX]], [[IMMUTABLE_TRIVIAL]], [[IMMUTABLE_RETAINABLE]], [[IMMUTABLE_AO_BOX]])
  // CHECK: apply {{.*}}[[CLOSURE]]

  // CHECK-NOT: strong_retain [[MUTABLE_TRIVIAL_BOX]]
  // CHECK-NOT: strong_retain [[MUTABLE_RETAINABLE_BOX]]
  // CHECK-NOT: strong_retain [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK-NOT: strong_retain [[IMMUTABLE_RETAINABLE]]
  // CHECK-NOT: strong_release [[IMMUTABLE_AO_BOX]]

  escape(captureEverything)

}

// CHECK: sil shared [[FN_NAME]] : $@convention(thin) (@guaranteed @box S, @guaranteed @box C, @guaranteed @box P, S, @guaranteed C, @guaranteed @box P)
