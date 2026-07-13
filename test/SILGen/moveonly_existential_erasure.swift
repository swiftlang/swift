// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

class Operation {}

func foo(_: borrowing AnyObject) {}

// compiler crash regression: https://github.com/swiftlang/swift/issues/86458
// CHECK-LABEL: sil hidden [ossa] @$s28moveonly_existential_erasure3bar9operationyAA9OperationC_tF : $@convention(thin) (@guaranteed Operation) -> () {
// CHECK: bb0([[ARG:%.*]] : @noImplicitCopy @guaranteed $Operation):
// CHECK: [[MOVEONLY_WRAPPER:%.*]] = copyable_to_moveonlywrapper [guaranteed] [[ARG]]
// CHECK: [[COPIED_WRAPPER:%.*]] = copy_value [[MOVEONLY_WRAPPER]]
// CHECK: [[MARKED_WRAPPER:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPIED_WRAPPER]]
func bar(operation: borrowing Operation) {
  // CHECK: [[BORROWED:%.*]] = begin_borrow [[MARKED_WRAPPER]]
  // CHECK: [[COPIED_BORROWED:%.*]] = copy_value [[BORROWED]]
  // CHECK: [[COPYABLE_WRAPPER:%.*]] = moveonlywrapper_to_copyable [owned] [[COPIED_BORROWED]]
  // CHECK: {{%.*}} = init_existential_ref [[COPYABLE_WRAPPER]] : $Operation : $Operation, $AnyObject
  foo(operation)

  // NOTE: the code above shouldn't crash but may fail move-only type checker:
  // `foo(operation)` is implicitly doing `foo(operation as AnyObject)`
  // and `operation` is consumed by the `as` operator, because `as` operator
  // hasn't been updated to work with borrowed values
  // SeeAlso: https://forums.swift.org/t/impossible-to-type-cast-or-test-non-copyable-existential-types/77160
}