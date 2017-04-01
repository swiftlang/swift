// RUN: %target-swift-frontend -parse-as-library -emit-silgen -enable-guaranteed-closure-contexts %s | %FileCheck %s

func use<T>(_: T) {}

func escape(_ f: () -> ()) {}

protocol P {}
class C: P {}
struct S {}

// CHECK-LABEL: sil hidden @_T026guaranteed_closure_context0A9_capturesyyF
func guaranteed_captures() {
  // CHECK: [[MUTABLE_TRIVIAL_BOX:%.*]] = alloc_box ${ var S }
  var mutableTrivial = S()
  // CHECK: [[MUTABLE_RETAINABLE_BOX:%.*]] = alloc_box ${ var C }
  var mutableRetainable = C()
  // CHECK: [[MUTABLE_ADDRESS_ONLY_BOX:%.*]] = alloc_box ${ var P }
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

  // CHECK-NOT: copy_value [[MUTABLE_TRIVIAL_BOX]]
  // CHECK-NOT: copy_value [[MUTABLE_RETAINABLE_BOX]]
  // CHECK-NOT: copy_value [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK-NOT: copy_value [[IMMUTABLE_RETAINABLE]]
  // CHECK:     [[IMMUTABLE_AO_BOX:%.*]] = alloc_box ${ var P }

  // CHECK: [[FN:%.*]] = function_ref [[FN_NAME:@_T026guaranteed_closure_context0A9_capturesyyF17captureEverythingL_yyF]]
  // CHECK: apply [[FN]]([[MUTABLE_TRIVIAL_BOX]], [[MUTABLE_RETAINABLE_BOX]], [[MUTABLE_ADDRESS_ONLY_BOX]], [[IMMUTABLE_TRIVIAL]], [[IMMUTABLE_RETAINABLE]], [[IMMUTABLE_AO_BOX]])
  captureEverything()

  // CHECK: destroy_value [[IMMUTABLE_AO_BOX]]

  // CHECK-NOT: copy_value [[MUTABLE_TRIVIAL_BOX]]
  // CHECK-NOT: copy_value [[MUTABLE_RETAINABLE_BOX]]
  // CHECK-NOT: copy_value [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK-NOT: copy_value [[IMMUTABLE_RETAINABLE]]

  // -- partial_apply still takes ownership of its arguments.
  // CHECK: [[FN:%.*]] = function_ref [[FN_NAME]]
  // CHECK: [[MUTABLE_TRIVIAL_BOX_COPY:%.*]] = copy_value [[MUTABLE_TRIVIAL_BOX]]
  // CHECK: [[MUTABLE_RETAINABLE_BOX_COPY:%.*]] = copy_value [[MUTABLE_RETAINABLE_BOX]]
  // CHECK: [[MUTABLE_ADDRESS_ONLY_BOX_COPY:%.*]] = copy_value [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK: [[IMMUTABLE_RETAINABLE_COPY:%.*]] = copy_value [[IMMUTABLE_RETAINABLE]]
  // CHECK: [[IMMUTABLE_AO_BOX:%.*]] = alloc_box ${ var P }
  // CHECK: [[CLOSURE:%.*]] = partial_apply {{.*}}([[MUTABLE_TRIVIAL_BOX_COPY]], [[MUTABLE_RETAINABLE_BOX_COPY]], [[MUTABLE_ADDRESS_ONLY_BOX_COPY]], [[IMMUTABLE_TRIVIAL]], [[IMMUTABLE_RETAINABLE_COPY]], [[IMMUTABLE_AO_BOX]])
  // CHECK: apply {{.*}}[[CLOSURE]]

  // CHECK-NOT: copy_value [[MUTABLE_TRIVIAL_BOX]]
  // CHECK-NOT: copy_value [[MUTABLE_RETAINABLE_BOX]]
  // CHECK-NOT: copy_value [[MUTABLE_ADDRESS_ONLY_BOX]]
  // CHECK-NOT: copy_value [[IMMUTABLE_RETAINABLE]]
  // CHECK-NOT: destroy_value [[IMMUTABLE_AO_BOX]]

  escape(captureEverything)

}

// CHECK: sil private [[FN_NAME]] : $@convention(thin) (@guaranteed { var S }, @guaranteed { var C }, @guaranteed { var P }, S, @guaranteed C, @guaranteed { var P })
