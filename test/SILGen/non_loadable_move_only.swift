// RUN: %target-swift-emit-silgen -module-name=test -primary-file %s | %FileCheck %s
// RUN: %target-swift-emit-sil -O -sil-verify-all %s

public struct GenericMoveOnly<T>: ~Copyable {
  var i: Int
  var s: T

  // CHECK-LABEL: sil [ossa] @$s4test15GenericMoveOnlyVfD : $@convention(method) <T> (@in GenericMoveOnly<T>) -> ()
  // CHECK:         [[STACK:%.*]] = alloc_stack
  // CHECK:         [[MARK:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[STACK]]
  // CHECK:         copy_addr [take] %0 to [init] [[MARK]]
  // CHECK:         [[DD:%.*]] = drop_deinit [[MARK]]
  // CHECK:         [[SE:%.*]] = struct_element_addr [[DD]]
  // CHECK-SAME:        #GenericMoveOnly.s
  // CHECK:         [[A:%.*]] = begin_access [deinit] [static] [[SE]]
  // CHECK:         destroy_addr [[A]]
  // CHECK:       } // end sil function '$s4test15GenericMoveOnlyVfD'
  deinit {
  }
}


