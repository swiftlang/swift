// Make sure project_box gets assigned the correct lexical scope when we create it.
// RUN: %target-swift-frontend -primary-file %s -Onone -emit-sil -Xllvm -sil-print-after=capture-promotion -Xllvm \
// RUN:   -sil-print-debuginfo -o /dev/null 2>&1 | %FileCheck %s

// CHECK: sil hidden @$s4null19captureStackPromoteSiycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> Int {
// CHECK: bb0:
// CHECK:   %0 = alloc_box ${ var Int }, var, name "x", loc {{.*}}:32:7, scope 3
// CHECK:   %1 = project_box %0 : ${ var Int }, 0, loc {{.*}}:32:7, scope 3
// CHECK:   %2 = metatype $@thin Int.Type, loc {{.*}}:32:11, scope 3
// CHECK:   %3 = integer_literal $Builtin.Int2048, 1, loc {{.*}}:32:11, scope 3
// CHECK:   %4 = function_ref @$sSi22_builtinIntegerLiteralSiBi2048__tcfC : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int, loc {{.*}}:32:11, scope 3
// CHECK:   %5 = apply %4(%3, %2) : $@convention(method) (Builtin.Int2048, @thin Int.Type) -> Int, loc {{.*}}:32:11, scope 3
// CHECK:   store %5 to [trivial] %1 : $*Int, loc {{.*}}:32:11, scope 3
// CHECK:   %7 = copy_value %0 : ${ var Int }, loc {{.*}}:33:11, scope 3
// CHECK:   %8 = project_box %7 : ${ var Int }, 0, loc {{.*}}:33:11, scope 3
// CHECK:   mark_function_escape %1 : $*Int, loc {{.*}}:33:11, scope 3
// CHECK:   %10 = function_ref @$s4null19captureStackPromoteSiycyFSiycfU_Tf2i_n : $@convention(thin) (Int) -> Int, loc {{.*}}:33:11, scope 3
// CHECK:   %11 = load [trivial] %8 : $*Int, loc {{.*}}:33:11, scope 3
// CHECK:   destroy_value %7 : ${ var Int }, loc {{.*}}:33:11, scope 3
// CHECK:   %13 = partial_apply [callee_guaranteed] %10(%11) : $@convention(thin) (Int) -> Int, loc {{.*}}:33:11, scope 3
// CHECK:   debug_value %13 : $@callee_guaranteed () -> Int, let, name "f", loc {{.*}}:33:7, scope 3
// CHECK:   %15 = begin_borrow %13 : $@callee_guaranteed () -> Int, loc {{.*}}:34:10, scope 3
// CHECK:   %16 = copy_value %15 : $@callee_guaranteed () -> Int, loc {{.*}}:34:10, scope 3
// CHECK:   end_borrow %15 : $@callee_guaranteed () -> Int
// CHECK:   destroy_value %13 : $@callee_guaranteed () -> Int, loc {{.*}}:35:1, scope 3
// CHECK:   destroy_value %0 : ${ var Int }, loc {{.*}}:35:1, scope 3
// CHECK:   return %16 : $@callee_guaranteed () -> Int, loc {{.*}}:34:3, scope 3
// CHECK: }


func captureStackPromote() -> () -> Int {
  var x = 1
  let f = { x }
  return f
}
