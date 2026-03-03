// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s

// Non-linearly recursive structures should not get optimized
class Tree {
  var left: Tree?
  var right: Tree?
}

// CHECK: sil hidden [ossa] @$s26deinit_recursive_branching4TreeCfd : $@convention(method) (@guaranteed Tree) -> @owned Builtin.NativeObject {
// CHECK: // [[SELF:%.*]] "self"
// CHECK: bb0([[SELF]] : @guaranteed $Tree):
// CHECK:   [[LEFT:%.*]] = ref_element_addr [[SELF]] : $Tree, #Tree.left
// CHECK:   [[LEFT_ACCESS:%.*]] = begin_access [deinit] [static] [[LEFT]] : $*Optional<Tree>
// CHECK:   destroy_addr [[LEFT_ACCESS]] : $*Optional<Tree>
// CHECK:   end_access [[LEFT_ACCESS]] : $*Optional<Tree>
// CHECK:   [[RIGHT:%.*]] = ref_element_addr [[SELF]] : $Tree, #Tree.right
// CHECK:   [[RIGHT_ACCESS:%.*]] = begin_access [deinit] [static] [[RIGHT]] : $*Optional<Tree>
// CHECK:   destroy_addr [[RIGHT_ACCESS]] : $*Optional<Tree>
// CHECK:   end_access [[RIGHT_ACCESS]] : $*Optional<Tree>                // id: %9
// CHECK:   [[SELF_NATIVE:%.*]] = unchecked_ref_cast [[SELF]] : $Tree to $Builtin.NativeObject
// CHECK:   [[SELF_NATIVE_OWNED:%.*]] = unchecked_ownership_conversion [[SELF_NATIVE]] : $Builtin.NativeObject, @guaranteed to @owned
// CHECK:   return [[SELF_NATIVE_OWNED]] : $Builtin.NativeObject
// CHECK: } // end sil function '$s26deinit_recursive_branching4TreeCfd'