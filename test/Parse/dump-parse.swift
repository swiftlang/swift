// RUN: %swift -dump-parse %s 2>&1 | FileCheck %s

// CHECK: (func_decl "foo"
func foo(n : Int) -> Int {
  // CHECK: (func_expr type='<null>'
  // CHECK:   (brace_stmt
  // CHECK:     (return_stmt
  // CHECK:       (integer_literal_expr type='<null>' value=42))))))
  return 42
}
