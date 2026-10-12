// RUN: %target-swift-emit-silgen %s -target %target-future-triple -enable-experimental-feature NoncopyableCasting | %FileCheck %s --check-prefix=ADDRESSES
// RUN: %target-swift-emit-silgen %s -target %target-future-triple -enable-sil-opaque-values -enable-experimental-feature NoncopyableCasting | %FileCheck %s --check-prefix=OPAQUE

// REQUIRES: swift_feature_NoncopyableCasting

// `case is T:` has to reach the non-consuming test in both SIL lowerings.
//
// With lowered addresses a borrowing subject is already in memory, so the cast
// reads it where it lies. With Opaque Values there are no addresses until
// AddressLowering runs, so emitSwitchStmt() hands the pattern machinery a value
// and it has to be borrowed into a temporary first -- otherwise the dispatch
// bails out and the ordinary lowering consumes a subject that cannot be
// consumed.
//
// Checked here rather than in an execution test because the lit mode that turns
// Opaque Values on, optimize_none_with_opaque_values, limits itself to
// `executable_test` and so skips test/SILGen entirely. A RUN line of our own
// exercises the lowering in the ordinary test run.

protocol P: ~Copyable {}

struct NC: P, ~Copyable { var t: Int }

// ADDRESSES-LABEL: sil hidden [ossa] @{{.*}}8classifyySiAA1P_pRi_s_XPF :
// ADDRESSES:       bb0(%0 : $*any P & ~Copyable):
// The subject is already memory, so the test reads it in place: no temporary.
// ADDRESSES-NOT:     alloc_stack
// ADDRESSES-NOT:     store_borrow
// ADDRESSES:         [[ACCESS:%[^ ]+]] = begin_access [read] [static] [no_nested_conflict]
// ADDRESSES:         checked_cast_addr_br test_only any P & ~Copyable in [[ACCESS]] to NC, bb1, bb2
// ADDRESSES:       } // end sil function

// OPAQUE-LABEL: sil hidden [ossa] [opaque] @{{.*}}8classifyySiAA1P_pRi_s_XPF :
// OPAQUE:       bb0(%0 : @guaranteed $any P & ~Copyable):
// The subject arrives as a value, so it is borrowed into a temporary and the
// borrow is ended on both edges out of the test -- nothing reads the memory
// once the question has been answered.
// OPAQUE:         [[TEMP:%[^ ]+]] = alloc_stack $any P & ~Copyable
// OPAQUE:         [[BORROW:%[^ ]+]] = store_borrow {{%[^ ]+}} to [[TEMP]]
// OPAQUE:         checked_cast_addr_br test_only any P & ~Copyable in [[BORROW]] to NC, bb1, bb2
// OPAQUE:       bb1:
// OPAQUE:         end_borrow [[BORROW]]
// OPAQUE:         dealloc_stack [[TEMP]]
// OPAQUE:       bb2:
// OPAQUE:         end_borrow [[BORROW]]
// OPAQUE:         dealloc_stack [[TEMP]]
// OPAQUE:       } // end sil function
func classify(_ box: borrowing any P & ~Copyable) -> Int {
  switch box {
  case is NC: return 1
  default: return 0
  }
}
