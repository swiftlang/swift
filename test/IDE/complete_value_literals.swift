// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=NIL_0 | FileCheck %s -check-prefix=NIL_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=NIL_1 | FileCheck %s -check-prefix=NIL_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=NIL_2 | FileCheck %s -check-prefix=NIL_2
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_0 | FileCheck %s -check-prefix=BOOL_0
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_1 | FileCheck %s -check-prefix=BOOL_1
// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=BOOL_2 | FileCheck %s -check-prefix=BOOL_2

struct MyNil1: NilLiteralConvertible {
  init(nilLiteral: ()) {}
}
struct MyBool1: BooleanLiteralConvertible {
  init(booleanLiteral value: Bool) {}
}

func testNil0() {
  let x: Int = #^NIL_0^#
}
// NIL_0: Keyword/None: nil;

func testNil1() {
  let x: MyNil1 = #^NIL_1^#
}
// NIL_1: Keyword/None/TypeRelation[Identical]: nil[#MyNil1#];

func testNil2() {
  let x: Int? = #^NIL_2^#
}
// NIL_2: Keyword/None/TypeRelation[Identical]: nil[#Int?#];

func testBool0() {
  let x: Int = #^BOOL_0^#
}
// BOOL_0: Keyword/None: true[#Bool#];
// BOOL_0: Keyword/None: false[#Bool#];

func testBool1() {
  let x: MyBool1 = #^BOOL_1^#
}
// BOOL_1: Keyword/None/TypeRelation[Identical]: true[#MyBool1#];
// BOOL_1: Keyword/None/TypeRelation[Identical]: false[#MyBool1#];

func testBool2() {
  let x: Bool = #^BOOL_2^#
}
// BOOL_2: Keyword/None/TypeRelation[Identical]: true[#Bool#];
// BOOL_2: Keyword/None/TypeRelation[Identical]: false[#Bool#];
