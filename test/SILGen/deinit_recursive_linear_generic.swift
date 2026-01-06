// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

class Node<A, B> {
  var next: Node<A, B>?
}

// CHECK: sil hidden [ossa] @$s31deinit_recursive_linear_generic4NodeCfd : $@convention(method) <A, B> (@guaranteed Node<A, B>) -> @owned Builtin.NativeObject {
// CHECK: [[SELF:%.*]] "self"
// CHECK: bb0([[SELF]] : @guaranteed $Node<A, B>):
// CHECK:   [[NIL:%.*]] = enum $Optional<Node<A, B>>, #Optional.none!enumelt
// CHECK:   [[SELF_NEXT:%.*]] = ref_element_addr [[SELF]] : $Node<A, B>, #Node.next
// CHECK:   [[ITER:%.*]] = alloc_stack $Optional<Node<A, B>>
// CHECK:   [[SELF_NEXT_ACCESS:%.*]] = begin_access [modify] [static] [no_nested_conflict] [[SELF_NEXT]] : $*Optional<Node<A, B>>
// CHECK:   [[SELF_NEXT_COPY:%.*]] = load [take] [[SELF_NEXT_ACCESS]] : $*Optional<Node<A, B>>
// CHECK:   store [[NIL]] to [init] [[SELF_NEXT_ACCESS]] : $*Optional<Node<A, B>>
// CHECK:   end_access [[SELF_NEXT_ACCESS]] : $*Optional<Node<A, B>>
// CHECK:   store [[SELF_NEXT_COPY]] to [init] [[ITER]] : $*Optional<Node<A, B>>
// CHECK:   br [[LOOPBB:bb.*]] //

// CHECK: [[LOOPBB]]:
// CHECK:   [[ITER_COPY:%.*]] = load [copy] [[ITER]] : $*Optional<Node<A, B>>
// CHECK:   switch_enum [[ITER_COPY]] : $Optional<Node<A, B>>, case #Optional.some!enumelt: [[IS_SOME_BB:bb.*]], case #Optional.none!enumelt: [[IS_NONE_BB:bb[0-9]+]]

// CHECK: [[IS_SOME_BB]]([[NODE:%.*]] : @owned $Node<A, B>):
// CHECK:   destroy_value [[NODE]] : $Node<A, B>
// CHECK:   [[IS_UNIQUE:%.*]] = is_unique [[ITER]] : $*Optional<Node<A, B>>
// CHECK:   cond_br [[IS_UNIQUE]], [[IS_UNIQUE_BB:bb.*]], [[NOT_UNIQUE_BB:bb[0-9]*]]

// CHECK: [[IS_UNIQUE_BB]]:
// CHECK:   [[ITER_BORROW:%.*]] = load_borrow [[ITER]] : $*Optional<Node<A, B>>
// CHECK:   [[ITER_UNWRAPPED:%.*]] = unchecked_enum_data [[ITER_BORROW]] : $Optional<Node<A, B>>, #Optional.some!enumelt
// CHECK:   [[NEXT_ADDR:%.*]] = ref_element_addr [[ITER_UNWRAPPED]] : $Node<A, B>, #Node.next
// CHECK:   [[NEXT_ADDR_ACCESS:%.*]] = begin_access [read] [static] [no_nested_conflict] [[NEXT_ADDR]] : $*Optional<Node<A, B>>
// CHECK:   [[NEXT_COPY:%.*]] = load [copy] [[NEXT_ADDR_ACCESS]] : $*Optional<Node<A, B>>
// CHECK:   end_access [[NEXT_ADDR_ACCESS]] : $*Optional<Node<A, B>>
// CHECK:   end_borrow [[ITER_BORROW]] : $Optional<Node<A, B>>
// CHECK:   store [[NEXT_COPY]] to [assign] [[ITER]] : $*Optional<Node<A, B>>
// CHECK:   br [[LOOPBB]]

// CHECK: [[NOT_UNIQUE_BB]]:
// CHECK:   br bb6

// CHECK: [[IS_NONE_BB]]:
// CHECK:   br [[CLEAN_BB:bb[0-9]+]]

// CHECK: [[CLEAN_BB]]:
// CHECK:   destroy_addr [[ITER]] : $*Optional<Node<A, B>>
// CHECK:   dealloc_stack [[ITER]] : $*Optional<Node<A, B>>
// CHECK:   [[SELF_NATIVE:%.*]] = unchecked_ref_cast [[SELF]] : $Node<A, B> to $Builtin.NativeObject
// CHECK:   [[SELF_NATIVE_OWNED:%.*]] = unchecked_ownership_conversion [[SELF_NATIVE]] : $Builtin.NativeObject, @guaranteed to @owned
// CHECK:   return [[SELF_NATIVE_OWNED]] : $Builtin.NativeObject
// CHECK: } // end sil function '$s31deinit_recursive_linear_generic4NodeCfd'


// Types of `self` and `next` don't match, so this should not be optimized
class Node2<A, B> {
  var next: Node2<Int, (A, B)>?
}

// CHECK: sil hidden [ossa] @$s31deinit_recursive_linear_generic5Node2Cfd : $@convention(method) <A, B> (@guaranteed Node2<A, B>) -> @owned Builtin.NativeObject {
// CHECK: [[SELF:%.*]] "self"
// CHECK: bb0([[SELF]] : @guaranteed $Node2<A, B>):
// CHECK:   debug_value [[SELF]] : $Node2<A, B>, let, name "self", argno 1
// CHECK:   [[NEXT_ADDR:%.*]] = ref_element_addr [[SELF]] : $Node2<A, B>, #Node2.next
// CHECK:   [[NEXT_ACCESS:%.*]] = begin_access [deinit] [static] [[NEXT_ADDR]] : $*Optional<Node2<Int, (A, B)>>
// CHECK:   destroy_addr [[NEXT_ACCESS]] : $*Optional<Node2<Int, (A, B)>>
// CHECK:   end_access [[NEXT_ACCESS]] : $*Optional<Node2<Int, (A, B)>>
// CHECK:   [[SELF_NATIVE:%.*]] = unchecked_ref_cast [[SELF]] : $Node2<A, B> to $Builtin.NativeObject
// CHECK:   [[SELF_OWNED:%.*]] = unchecked_ownership_conversion [[SELF_NATIVE]] : $Builtin.NativeObject, @guaranteed to @owned
// CHECK:   return [[SELF_OWNED]] : $Builtin.NativeObject
// CHECK: } // end sil function '$s31deinit_recursive_linear_generic5Node2Cfd'
