// RUN: %target-swift-frontend -O -emit-sil %s | FileCheck %s

func bar(x : Int) { }

// CHECK-LABEL: _TF6no_opt4foo1FT_T_
// CHECK-NOT: integer_literal
// CHECK: return
public func foo1() {
  bar(1)
  bar(2)
  bar(3)
  bar(4)
}

// CHECK-LABEL: _TF6no_opt4foo2FT_T_
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: return
@semantics("optimize.sil.never")
public func foo2() {
  bar(1)
  bar(2)
  bar(3)
  bar(4)
}
