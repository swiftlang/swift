// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

func bar(_ x : Int) { }

// CHECK-LABEL: $s6no_opt4foo1yyF
// CHECK-NOT: integer_literal
// CHECK: return
public func foo1() {
  bar(1)
  bar(2)
  bar(3)
  bar(4)
}

// CHECK-LABEL: $s6no_opt4foo2yyF
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: return
@_optimize(none)
public func foo2() {
  bar(1)
  bar(2)
  bar(3)
  bar(4)
}
