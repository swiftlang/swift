// RUN: %target-swift-emit-silgen                            \
// RUN:     %s                                               \
// RUN:     -enable-callee-allocated-coro-abi                \
// RUN:     -experimental-skip-non-inlinable-function-bodies \
// RUN:     -enable-library-evolution                        \
// RUN:     -enable-experimental-feature CoroutineAccessors  \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-NOUNWIND

// RUN: %target-swift-emit-silgen                                              \
// RUN:     %s                                                                 \
// RUN:     -enable-callee-allocated-coro-abi                                  \
// RUN:     -experimental-skip-non-inlinable-function-bodies                   \
// RUN:     -enable-library-evolution                                          \
// RUN:     -enable-experimental-feature CoroutineAccessors                    \
// RUN:     -enable-experimental-feature CoroutineAccessorsUnwindOnCallerError \
// RUN: | %FileCheck %s --check-prefixes=CHECK,CHECK-UNWIND

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_CoroutineAccessorsUnwindOnCallerError

// CHECK-LABEL: sil_default_witness_table MutatableAssociatedField {
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #MutatableAssociatedField.field!read2
// CHECK-SAME:        : @$s24coroutine_accessors_skip24MutatableAssociatedFieldP5field5AssocQzvy
// CHECK-NEXT:    no_default
// CHECK-NEXT:    no_default
// CHECK-NEXT:    method #MutatableAssociatedField.field!modify2
// CHECK-SAME:        : @$s24coroutine_accessors_skip24MutatableAssociatedFieldP5field5AssocQzvx
// CHECK-NEXT:  }
public protocol MutatableAssociatedField {
  associatedtype Assoc

  var field: Assoc { read set }
}
