// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class Node {
  var elem: [Int64] = []
  var next: Node?
}

// CHECK: sil hidden [ossa] @$s23deinit_recursive_linear4NodeCfd : $@convention(method) (@guaranteed Node) -> @owned Builtin.NativeObject {
// CHECK: [[SELF:%.*]] "self"
// CHECK: bb0([[SELF]] : @guaranteed $Node):
// CHECK:   [[ELEM:%.*]] = ref_element_addr [[SELF]] : $Node, #Node.elem
// CHECK:   [[ELEM_ACCESS:%.*]] = begin_access [deinit] [static] [[ELEM]] : $*Array<Int64>
// CHECK:   destroy_addr [[ELEM_ACCESS]] : $*Array<Int64>
// CHECK:   end_access [[ELEM_ACCESS]] : $*Array<Int64>
// CHECK:   [[NIL:%.*]] = enum $Optional<Node>, #Optional.none!enumelt
// CHECK:   [[SELF_NEXT:%.*]] = ref_element_addr [[SELF]] : $Node, #Node.next
// CHECK:   [[ITER:%.*]] = alloc_stack $Optional<Node>
// CHECK:   [[SELF_NEXT_ACCESS:%.*]] = begin_access [modify] [static] [[SELF_NEXT]] : $*Optional<Node>
// CHECK:   [[SELF_NEXT_COPY:%.*]] = load [copy] [[SELF_NEXT_ACCESS]] : $*Optional<Node>
// CHECK:   store [[NIL]] to [assign] [[SELF_NEXT_ACCESS]] : $*Optional<Node>
// CHECK:   end_access [[SELF_NEXT_ACCESS]] : $*Optional<Node>
// CHECK:   store [[SELF_NEXT_COPY]] to [init] [[ITER]] : $*Optional<Node>
// CHECK:   br [[LOOPBB:bb.*]] //

// CHECK: [[LOOPBB]]:
// CHECK:   [[ITER_ADDR:%.*]] = load [copy] [[ITER]] : $*Optional<Node>
// CHECK:   [[ITER_COPY:%.*]] = copy_value [[ITER_ADDR]] : $Optional<Node>
// CHECK:   [[ITER_COPY_ADDR:%.*]] = alloc_stack $Optional<Node>
// CHECK:   store [[ITER_COPY]] to [init] [[ITER_COPY_ADDR]] : $*Optional<Node>
// CHECK:   destroy_value [[ITER_ADDR]] : $Optional<Node>
// CHECK:   switch_enum_addr [[ITER_COPY_ADDR]] : $*Optional<Node>, case #Optional.some!enumelt: [[IS_SOME_BB:bb.*]], case #Optional.none!enumelt: [[IS_NONE_BB:bb[0-9]+]]

// CHECK: [[IS_SOME_BB]]:
// CHECK:   destroy_addr [[ITER_COPY_ADDR]] : $*Optional<Node>
// CHECK:   dealloc_stack [[ITER_COPY_ADDR]] : $*Optional<Node>
// CHECK:   [[IS_UNIQUE:%.*]] = is_unique [[ITER]] : $*Optional<Node>
// CHECK:   cond_br [[IS_UNIQUE]], [[IS_UNIQUE_BB:bb.*]], [[NOT_UNIQUE_BB:bb[0-9]*]]

// CHECK: [[IS_UNIQUE_BB]]:
// CHECK:   [[ITER_ADDR:%.*]] = load [copy] [[ITER]] : $*Optional<Node>
// CHECK:   [[ITER_BORROW:%.*]] = begin_borrow [[ITER_ADDR]] : $Optional<Node>
// CHECK:   [[ITER_UNWRAPPED:%.*]] = unchecked_enum_data [[ITER_BORROW]] : $Optional<Node>, #Optional.some!enumelt
// CHECK:   [[NEXT_ADDR:%.*]] = ref_element_addr [[ITER_UNWRAPPED]] : $Node, #Node.next
// CHECK:   [[NEXT_ADDR_ACCESS:%.*]] = begin_access [read] [static] [[NEXT_ADDR]] : $*Optional<Node>
// CHECK:   [[NEXT_COPY:%.*]] = load [copy] [[NEXT_ADDR_ACCESS]] : $*Optional<Node>
// CHECK:   end_access [[NEXT_ADDR_ACCESS]] : $*Optional<Node>
// CHECK:   store [[NEXT_COPY]] to [assign] [[ITER]] : $*Optional<Node>
// CHECK:   end_borrow [[ITER_BORROW]] : $Optional<Node>
// CHECK:   destroy_value [[ITER_ADDR]] : $Optional<Node>
// CHECK:   br [[LOOPBB]]

// CHECK: [[NOT_UNIQUE_BB]]:
// CHECK:   br bb6

// CHECK: [[IS_NONE_BB]]:
// CHECK:   dealloc_stack [[ITER_COPY_ADDR]] : $*Optional<Node>
// CHECK:   br [[CLEAN_BB:bb[0-9]+]]

// CHECK: [[CLEAN_BB]]:
// CHECK:   destroy_addr [[ITER]] : $*Optional<Node>
// CHECK:   dealloc_stack [[ITER]] : $*Optional<Node>
// CHECK:   [[SELF_NATIVE:%.*]] = unchecked_ref_cast [[SELF]] : $Node to $Builtin.NativeObject
// CHECK:   [[SELF_NATIVE_OWNED:%.*]] = unchecked_ownership_conversion [[SELF_NATIVE]] : $Builtin.NativeObject, @guaranteed to @owned
// CHECK:   return [[SELF_NATIVE_OWNED]] : $Builtin.NativeObject
// CHECK: } // end sil function '$s23deinit_recursive_linear4NodeCfd'
