// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -emit-sil %s | %FileCheck %s

func bar(_ x : Int) { }

// CHECK-LABEL: _T06no_opt4foo1yyF
// CHECK-NOT: integer_literal
// CHECK: return
public func foo1() {
  bar(1)
  bar(2)
  bar(3)
  bar(4)
}

// CHECK-LABEL: _T06no_opt4foo2yyF
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: integer_literal
// CHECK: return
@_semantics("optimize.sil.never")
public func foo2() {
  bar(1)
  bar(2)
  bar(3)
  bar(4)
}
