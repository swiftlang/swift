// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -sil-verify-all -o /dev/null %s

// rdar://185741325
//
// Borrow accessors called on an owned rvalue ~Copyable base must borrow the
// base for the call, instead of passing the owned value as a @guaranteed
// operand.

public final class Klass {
  var id = 0
}

public struct NC: ~Copyable {
  var id = 0
}

func use(_ nc: borrowing NC) {}

public struct Inner: ~Copyable {
  var _k: Klass
  var _nc: NC

  var k: Klass {
    borrow { return _k }
  }

  var nc: NC {
    borrow { return _nc }
  }
}

public struct Outer: ~Copyable {
  var _inner: Inner

  var inner: Inner {
    borrow { return _inner }
  }
}

func makeInner() -> Inner { Inner(_k: Klass(), _nc: NC()) }
func makeOuter() -> Outer { Outer(_inner: makeInner()) }

// CHECK-LABEL: sil hidden [ossa] @$s27borrow_accessor_rvalue_base8initBaseSiyF :
// CHECK:   [[INNER:%.*]] = apply {{%.*}}({{%.*}}, {{%.*}}, {{%.*}}) : $@convention(method) (@owned Klass, @owned NC, @thin Inner.Type) -> @owned Inner
// CHECK:   [[BORROW:%.*]] = begin_borrow [[INNER]]
// CHECK-NOT: copy_value
// CHECK-NOT: mark_unresolved_non_copyable_value
// CHECK:   [[FN:%.*]] = function_ref @$s27borrow_accessor_rvalue_base5InnerV1kAA5KlassCvb :
// CHECK-NOT: apply [[FN]]([[INNER]])
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK:   end_borrow [[BORROW]]
// CHECK:   destroy_value [[INNER]]
// CHECK: } // end sil function '$s27borrow_accessor_rvalue_base8initBaseSiyF'
func initBase() -> Int {
  let k = Inner(_k: Klass(), _nc: NC()).k
  return k.id
}

// CHECK-LABEL: sil hidden [ossa] @$s27borrow_accessor_rvalue_base17noncopyableResultyyF :
// CHECK:   [[INNER:%.*]] = apply {{%.*}}() : $@convention(thin) () -> @owned Inner
// CHECK:   [[BORROW:%.*]] = begin_borrow [[INNER]]
// CHECK-NOT: copy_value
// CHECK-NOT: mark_unresolved_non_copyable_value
// CHECK:   [[FN:%.*]] = function_ref @$s27borrow_accessor_rvalue_base5InnerV2ncAA2NCVvb :
// CHECK-NOT: apply [[FN]]([[INNER]])
// CHECK:   apply [[FN]]([[BORROW]])
// CHECK: } // end sil function '$s27borrow_accessor_rvalue_base17noncopyableResultyyF'
func noncopyableResult() {
  use(makeInner().nc)
}

// SILGen wraps the noncopyable result of Outer.inner in copy_value +
// mark_unresolved_non_copyable_value + begin_borrow for the move-only checker.
// Inner.nc is still called directly on the result of Outer.inner rather than on
// that begin_borrow. The begin_borrow of the owned makeOuter() result is kept
// as the self operand of Outer.inner.
//
// CHECK-LABEL: sil hidden [ossa] @$s27borrow_accessor_rvalue_base7chainedyyF :
// CHECK:   [[OUTER:%.*]] = apply {{%.*}}() : $@convention(thin) () -> @owned Outer
// CHECK:   [[BORROW:%.*]] = begin_borrow [[OUTER]]
// CHECK-NOT: copy_value
// CHECK-NOT: mark_unresolved_non_copyable_value
// CHECK:   [[INNERFN:%.*]] = function_ref @$s27borrow_accessor_rvalue_base5OuterV5innerAA5InnerVvb :
// CHECK:   [[INNER:%.*]] = apply [[INNERFN]]([[BORROW]])
// CHECK:   [[COPY:%.*]] = copy_value [[INNER]]
// CHECK:   [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[COPY]]
// CHECK:   [[INNER_BORROW:%.*]] = begin_borrow [[MARK]]
// CHECK:   [[NCFN:%.*]] = function_ref @$s27borrow_accessor_rvalue_base5InnerV2ncAA2NCVvb :
// CHECK-NOT: apply [[NCFN]]([[INNER_BORROW]])
// CHECK:   apply [[NCFN]]([[INNER]])
// CHECK: } // end sil function '$s27borrow_accessor_rvalue_base7chainedyyF'
func chained() {
  use(makeOuter().inner.nc)
}

func chainedCopyable() -> Int {
  return makeOuter().inner.k.id
}
