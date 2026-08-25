// RUN: %target-swift-emit-silgen -module-name type_of -Xllvm -sil-print-types %s | %FileCheck %s

// `type(of:)` lowers to `value_metatype`/`existential_metatype`, neither of
// which consumes its operand. So for an address-only operand we borrow the
// storage in place rather than copying it into a temporary to form an rvalue.
// The copy would be a consume, which is what made `type(of:)` unusable on
// noncopyable values.

protocol P: ~Copyable {}

// CHECK-LABEL: sil hidden [ossa] @$s7type_of16genericBorrowingyxmxRi_zlF : $@convention(thin) <T where T : ~Copyable> (@in_guaranteed T) -> @thick T.Type {
// CHECK:       bb0([[V:%.*]] : $*T):
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     copy_addr
// CHECK:         [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[V]]
// CHECK-NEXT:    [[META:%.*]] = value_metatype $@thick T.Type, [[MARK]]
// CHECK-NEXT:    return [[META]]
// CHECK:       } // end sil function '$s7type_of16genericBorrowingyxmxRi_zlF'
func genericBorrowing<T: ~Copyable>(_ v: borrowing T) -> T.Type {
  return type(of: v)
}

// A `consuming` parameter gets a box, so the read goes through an access scope
// -- but still no copy, and the access ends right after the metatype is derived.
//
// CHECK-LABEL: sil hidden [ossa] @$s7type_of16genericConsumingyxmxnRi_zlF : $@convention(thin) <T where T : ~Copyable> (@in T) -> @thick T.Type {
// CHECK:         [[BOX:%.*]] = project_box
// CHECK:         [[ACCESS:%.*]] = begin_access [read] [unknown] [[BOX]]
// CHECK-NEXT:    [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[ACCESS]]
// CHECK-NEXT:    [[META:%.*]] = value_metatype $@thick T.Type, [[MARK]]
// CHECK-NEXT:    end_access [[ACCESS]]
// CHECK:       } // end sil function '$s7type_of16genericConsumingyxmxnRi_zlF'
func genericConsuming<T: ~Copyable>(_ v: consuming T) -> T.Type {
  return type(of: v)
}

// A noncopyable existential goes through `existential_metatype` instead, and is
// likewise only borrowed.
//
// CHECK-LABEL: sil hidden [ossa] @$s7type_of20existentialBorrowingyAA1P_pRi_s_XPXpAaC_pRi_s_XPF : $@convention(thin) (@in_guaranteed any P & ~Copyable) -> @thick any (P & ~Copyable).Type {
// CHECK:       bb0([[B:%.*]] : $*any P & ~Copyable):
// CHECK-NOT:     alloc_stack
// CHECK-NOT:     copy_addr
// CHECK:         [[MARK:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[B]]
// CHECK-NEXT:    [[META:%.*]] = existential_metatype $@thick any (P & ~Copyable).Type, [[MARK]]
// CHECK-NEXT:    return [[META]]
// CHECK:       } // end sil function '$s7type_of20existentialBorrowingyAA1P_pRi_s_XPXpAaC_pRi_s_XPF'
func existentialBorrowing(
  _ b: borrowing any P & ~Copyable
) -> any (P & ~Copyable).Type {
  return type(of: b)
}

// A copyable class operand is loadable, but reading its type still doesn't need
// a retain: the metatype is derived from the borrowed storage.
//
// CHECK-LABEL: sil hidden [ossa] @$s7type_of12classOperandyyXlXpyXlF : $@convention(thin) (@guaranteed AnyObject) -> @thick any AnyObject.Type {
// CHECK-NOT:     copy_value
// CHECK:         existential_metatype $@thick any AnyObject.Type
// CHECK:       } // end sil function '$s7type_of12classOperandyyXlXpyXlF'
func classOperand(_ o: AnyObject) -> AnyObject.Type {
  return type(of: o)
}
