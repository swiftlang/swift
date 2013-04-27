// RUN: %swift -dump-parse %s 2>&1 | FileCheck %s
func r13756261(c0:UInt8) -> Int {
  // CHECK: (if_expr
  // CHECK:   (paren_expr
  // CHECK:   (integer_literal_expr
  // CHECK:   (if_expr
  // CHECK:     (paren_expr
  // CHECK:     (integer_literal_expr
  // CHECK:     (if_expr
  // CHECK:       (paren_expr
  // CHECK:       (integer_literal_expr
  // CHECK:       (integer_literal_expr
  return (c0 < 0x80) ? 1 : (c0 < 0xE0) ? 2 : (c0 < 0xF0) ? 3 : 4
}
