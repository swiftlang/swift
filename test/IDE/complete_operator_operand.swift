// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BINARY_RHS_1 | %FileCheck %s -check-prefix=BINARY_RHS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BINARY_RHS_2 | %FileCheck %s -check-prefix=BINARY_RHS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PREFIX_1 | %FileCheck %s -check-prefix=PREFIX_1

infix operator <-->
prefix operator -->

class C1 {}
class C2 {}
func <--> (lhs: C1, rhs: C2) -> Bool {}
func <--> (lhs: C2, rhs: C1) -> Bool {}

func test_binary1(c1: C1, c2: C2) {
  _ = c1 <--> #^BINARY_RHS_1^#
// BINARY_RHS_1: Begin completion
// BINARY_RHS_1-NOT: Decl[LocalVar]/Local/TypeRelation[Identical]: c1[#C1#]; name=c1
// BINARY_RHS_1-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: c2[#C2#]; name=c2

  _ = c2 <--> #^BINARY_RHS_2^#
// BINARY_RHS_2: Begin completion
// BINARY_RHS_2-NOT: Decl[LocalVar]/Local/TypeRelation[Identical]: c2[#C2#]; name=c2
// BINARY_RHS_2-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: c1[#C1#]; name=c1
}

prefix func --> (operand: C1) -> Bool {}
prefix func --> (operand: C2) -> Bool {}

func test_prefix1(c1: C1, c2: C2) {
  _ = -->#^PREFIX_1^#
// PREFIX_1: Begin completion
// PREFIX_1-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: c1[#C1#]; name=c1
// PREFIX_1-DAG: Decl[LocalVar]/Local/TypeRelation[Identical]: c2[#C2#]; name=c2
}
