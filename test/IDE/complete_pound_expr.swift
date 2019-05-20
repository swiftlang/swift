// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_EXPR_1 | %FileCheck %s -check-prefix=POUND_EXPR_STRINGCONTEXT
// REQUIRES: objc_interop

func use(_ str: String) -> Bool {}

func test1() {
  _ = use(##^POUND_EXPR_1^#)
}

// POUND_EXPR_STRINGCONTEXT: Begin completions, 7 items
// POUND_EXPR_STRINGCONTEXT-NOT: available
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#function]/None/TypeRelation[Identical]: function[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#file]/None/TypeRelation[Identical]: file[#String#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#line]/None:                line[#Int#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#column]/None:              column[#Int#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword[#dsohandle]/None:           dsohandle[#UnsafeRawPointer#];
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword/ExprSpecific:               selector({#@objc method#});
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword/ExprSpecific:               keyPath({#@objc property sequence#});
// POUND_EXPR_STRINGCONTEXT: End completions
