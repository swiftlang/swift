// RUN: %target-swift-frontend -dump-ast %s 2>&1 | %FileCheck %s -check-prefix=CHECK-AST
// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s -check-prefix=CHECK-SIL

func thin(x: Float) -> Float { return x }

func myfunction(_ x: @escaping @autodiff (Float) -> (Float)) -> (Float) -> Float {
  return x
}

func apply() {
  _ = myfunction(thin)
}

// CHECK-AST-LABEL:  (func_decl {{.*}} "myfunction(_:)"
// CHECK-AST:          (return_stmt
// CHECK-AST-NEXT:       (function_conversion_expr implicit type='(Float) -> Float'
// CHECK-AST-NEXT:         (autodiff_function_extract_original implicit type='(Float) -> (Float)'
// CHECK-AST-NEXT:           (declref_expr type='@autodiff (Float) -> (Float)'
// CHECK-AST-LABEL:  (func_decl {{.*}} "apply()"
// CHECK-AST:          (function_conversion_expr implicit type='@autodiff (Float) -> (Float)'
// CHECK-AST-NEXT:       (autodiff_function implicit type='@autodiff (Float) -> Float'
// CHECK-AST-NEXT:         (declref_expr type='(Float) -> Float'

// CHECK-SIL-LABEL: @{{.*}}myfunction{{.*}}
// CHECK-SIL: bb0([[DIFFED:%.*]] : @guaranteed $@autodiff @callee_guaranteed (Float) -> Float):
// CHECK-SIL:   [[DIFFED_COPY:%.*]] = copy_value [[DIFFED]] : $@autodiff @callee_guaranteed (Float) -> Float
// CHECK-SIL:   [[ORIG:%.*]] = autodiff_function_extract [original] [[DIFFED_COPY]] : $@autodiff @callee_guaranteed (Float) -> Float
// CHECK-SIL:   return [[ORIG]] : $@callee_guaranteed (Float) -> Float

// CHECK-SIL-LABEL: @{{.*}}apply{{.*}}
// CHECK-SIL:       [[ORIG:%.*]] = function_ref @{{.*}}thin{{.*}} : $@convention(thin) (Float) -> Float
// CHECK-SIL-NEXT:  [[ORIG_THICK:%.*]] = thin_to_thick_function [[ORIG]] : $@convention(thin) (Float) -> Float to $@callee_guaranteed (Float) -> Float
// CHECK-SIL-NEXT:  [[DIFFED:%.*]] = autodiff_function [wrt 0] [order 1] %1 : $@callee_guaranteed (Float) -> Float
// CHECK-SIL:   destroy_value [[DIFFED]] : $@autodiff @callee_guaranteed (Float) -> Float

