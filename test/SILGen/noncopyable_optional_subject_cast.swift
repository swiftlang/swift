// RUN: %target-swift-emit-silgen %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting -o /dev/null
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting -o /dev/null

// REQUIRES: swift_feature_NoncopyableCasting

// A cast whose subject is an Optional of a non-`Copyable` type is a cast of the
// Optional itself (Sema doesn't unwrap it), so the runtime unwraps the payload
// and `nil` fails. A test borrows the subject (`test_only`); a binding takes it
// only on success (`take_on_success`). Nothing unwraps the Optional with a
// `load [take]` or a whole-subject `copy_addr [take]` before testing.

protocol P: ~Copyable {}
protocol Q: ~Copyable {}
struct NC: P, ~Copyable {}

// CHECK-LABEL: sil hidden [ossa] @$s33noncopyable_optional_subject_cast11isBorrowing{{[_0-9a-zA-Z]*}}F
// CHECK:         [[O:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] %0
// CHECK:         checked_cast_addr_br test_only Optional<any P & ~Copyable> in [[O]] to NC
// CHECK-NOT:     copy_addr
// CHECK-NOT:     destroy_addr
// CHECK:       } // end sil function
func isBorrowing(_ o: borrowing (any P & ~Copyable)?) -> Bool {
  o is NC
}

// CHECK-LABEL: sil hidden [ossa] @$s33noncopyable_optional_subject_cast5ifLet{{[_0-9a-zA-Z]*}}F
// CHECK:         [[O:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable]
// CHECK:         checked_cast_addr_br take_on_success Optional<any P & ~Copyable> in [[O]] to NC in
// CHECK:       } // end sil function
func ifLet(_ o: consuming (any P & ~Copyable)?) -> Bool {
  if let v = o as? NC { _ = v; return true }
  _ = consume o
  return false
}

// CHECK-LABEL: sil hidden [ossa] @$s33noncopyable_optional_subject_cast8ifCaseIs{{[_0-9a-zA-Z]*}}F
// CHECK:         [[O:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] %0
// CHECK:         checked_cast_addr_br test_only Optional<any P & ~Copyable> in [[O]] to any Q & ~Copyable
// CHECK-NOT:     copy_addr
// CHECK-NOT:     destroy_addr
// CHECK:       } // end sil function
func ifCaseIs(_ o: borrowing (any P & ~Copyable)?) -> Bool {
  if case is any Q & ~Copyable = o { return true }
  return false
}

// CHECK-LABEL: sil hidden [ossa] @$s33noncopyable_optional_subject_cast9ifCaseLet{{[_0-9a-zA-Z]*}}F
// CHECK:         [[O:%.*]] = mark_unresolved_non_copyable_value [assignable_but_not_consumable]
// CHECK:         checked_cast_addr_br take_on_success Optional<any P & ~Copyable> in [[O]] to NC in
// CHECK:       } // end sil function
func ifCaseLet(_ o: consuming (any P & ~Copyable)?) -> Bool {
  if case let v as NC = o { _ = v; return true }
  _ = consume o
  return false
}
