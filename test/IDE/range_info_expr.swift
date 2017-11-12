struct S {
  let count = 1
}

public class CC {
  func foo(_ s : S) -> Int {
    if s.count > 0 {
      return 1
    } else {
      return 0
    }
  }
  func getSelf() -> CC {
    return self
  }
  func getSelf(_ i : Int) -> CC {
    return self
  }
}

func foo1(_ c : CC) -> CC{
  _ = c.getSelf().getSelf().getSelf().getSelf()
  _ = c.getSelf(1).getSelf(1).getSelf(1).getSelf(1)
  return c.getSelf()
}

// RUN: %target-swift-ide-test -range -pos=7:8 -end-pos=7:19 -source-filename %s | %FileCheck %s -check-prefix=CHECK-BOOL
// CHECK-BOOL: <Type>Bool</Type>

// RUN: %target-swift-ide-test -range -pos=22:39 -end-pos=22:48 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR
// RUN: %target-swift-ide-test -range -pos=22:29 -end-pos=22:38 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR
// RUN: %target-swift-ide-test -range -pos=22:19 -end-pos=22:28 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR
// RUN: %target-swift-ide-test -range -pos=22:9 -end-pos=22:18 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR

// RUN: %target-swift-ide-test -range -pos=23:42 -end-pos=23:52 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR1
// RUN: %target-swift-ide-test -range -pos=23:31 -end-pos=23:41 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR1
// RUN: %target-swift-ide-test -range -pos=23:20 -end-pos=23:30 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR1
// RUN: %target-swift-ide-test -range -pos=23:9 -end-pos=23:19 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PART-EXPR1

// CHECK-PART-EXPR: <Kind>PartOfExpression</Kind>
// CHECK-PART-EXPR-NEXT: <Content>getSelf()</Content>
// CHECK-PART-EXPR-NEXT: <Context>swift_ide_test.(file).foo1(_:)</Context>
// CHECK-PART-EXPR-NEXT: <Parent>Call</Parent>
// CHECK-PART-EXPR-NEXT: <ASTNodes>2</ASTNodes>
// CHECK-PART-EXPR-NEXT: <end>

// CHECK-PART-EXPR1: <Kind>PartOfExpression</Kind>
// CHECK-PART-EXPR2: <Content>getSelf(1)</Content>
// CHECK-PART-EXPR2: <Context>swift_ide_test.(file).foo1(_:)</Context>
// CHECK-PART-EXPR2: <Parent>Call</Parent>
// CHECK-PART-EXPR2: <ASTNodes>2</ASTNodes>
// CHECK-PART-EXPR2: <end>
