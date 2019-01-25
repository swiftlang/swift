// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s -check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s -check-prefix=CHECK-SILGEN
// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s -check-prefix=CHECK-SIL

func thin(x: Float) -> Float { return x }

func myfunction(_ f: @escaping @autodiff (Float) -> (Float)) -> (Float) -> Float {
  // @autodiff functions should be callable.
  _ = f(.zero)
  return f
}

func apply() {
  _ = myfunction(thin)
}

// CHECK-AST-LABEL:  (func_decl {{.*}} "myfunction(_:)"
// CHECK-AST:          (call_expr type='(Float)'
// CHECK-AST:            (autodiff_function_extract_original implicit type='(Float) -> (Float)'
// CHECK-AST:              (declref_expr type='@autodiff (Float) -> (Float)'
// CHECK-AST:          (return_stmt
// CHECK-AST:            (function_conversion_expr implicit type='(Float) -> Float'
// CHECK-AST:              (autodiff_function_extract_original implicit type='(Float) -> (Float)'
// CHECK-AST:                (declref_expr type='@autodiff (Float) -> (Float)'
// CHECK-AST-LABEL:  (func_decl {{.*}} "apply()"
// CHECK-AST:          (autodiff_function implicit type='@autodiff (Float) -> (Float)'
// CHECK-AST:            (function_conversion_expr implicit type='(Float) -> (Float)'
// CHECK-AST:              (declref_expr type='(Float) -> Float'

// CHECK-SILGEN-LABEL: @{{.*}}myfunction{{.*}}
// CHECK-SILGEN: bb0([[DIFFED:%.*]] : @guaranteed $@autodiff @callee_guaranteed (Float) -> Float):
// CHECK-SILGEN:   [[DIFFED_COPY:%.*]] = copy_value [[DIFFED]] : $@autodiff @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[ORIG:%.*]] = autodiff_function_extract [original] [[DIFFED_COPY]] : $@autodiff @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[BORROWED_ORIG:%.*]] = begin_borrow [[ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   apply [[BORROWED_ORIG]]({{%.*}}) : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   destroy_value [[ORIG]] : $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[DIFFED_COPY:%.*]] = copy_value [[DIFFED]] : $@autodiff @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   [[ORIG:%.*]] = autodiff_function_extract [original] [[DIFFED_COPY]] : $@autodiff @callee_guaranteed (Float) -> Float
// CHECK-SILGEN:   return [[ORIG]] : $@callee_guaranteed (Float) -> Float

// CHECK-SILGEN-LABEL: @{{.*}}apply{{.*}}
// CHECK-SILGEN:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-SILGEN-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-SILGEN-NEXT:  [[DIFFED:%.*]] = autodiff_function [wrt 0] [order 1] [[ORIG_THICK]] : $@callee_guaranteed (Float) -> Float

// CHECK-SIL:  [[DIFFED:%.*]] = autodiff_function [wrt 0] [order 1] {{%.*}} : $@callee_guaranteed (Float) -> Float
// CHECK-SIL:  release_value [[DIFFED]] : $@autodiff @callee_guaranteed (Float) -> Float
