// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POUND_EXPR_1 | %FileCheck %s -check-prefix=POUND_EXPR_STRINGCONTEXT
// REQUIRES: objc_interop

func use(_ str: String) -> Bool {}

func test1() {
  _ = use(##^POUND_EXPR_1^#)
}

// POUND_EXPR_STRINGCONTEXT: Begin completions, 2 items
// POUND_EXPR_STRINGCONTEXT-NOT: available
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword/ExprSpecific: selector({#@objc method#}); name=selector(@objc method)
// POUND_EXPR_STRINGCONTEXT-DAG: Keyword/ExprSpecific: keyPath({#@objc property sequence#}); name=keyPath(@objc property sequence)
// POUND_EXPR_STRINGCONTEXT: End completions
