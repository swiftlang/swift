// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_1 | %FileCheck %s -check-prefix=GLOBAL_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_2 | %FileCheck %s -check-prefix=GLOBAL_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_3 | %FileCheck %s -check-prefix=GLOBAL_FUNC_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_4 | %FileCheck %s -check-prefix=GLOBAL_FUNC_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC_5 | %FileCheck %s -check-prefix=GLOBAL_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD_1 | %FileCheck %s -check-prefix=METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD_2 | %FileCheck %s -check-prefix=METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD_3 | %FileCheck %s -check-prefix=METHOD_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD_4 | %FileCheck %s -check-prefix=METHOD_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD_5 | %FileCheck %s -check-prefix=METHOD_1

// XFAIL: *

func globalFunc1(fn1: () -> Int, fn2: () -> String) {}
func testGlobalFunc() {
  globalFunc1 {
    #^GLOBAL_FUNC_1^#
// GLOBAL_FUNC_1: Begin completions, 1 items
// GLOBAL_FUNC_1-DAG: Pattern/ExprSpecific: {#fn1: () -> Int##() -> Int#}[#() -> Int#];
// GLOBAL_FUNC_1: End completions
  }
  globalFunc1() {
    #^GLOBAL_FUNC_2^#
// Same as GLOBAL_FUNC_1
  }
  globalFunc1() {
    fn1: { 1 }
    #^GLOBAL_FUNC_3^#
// GLOBAL_FUNC_3: Begin completions, 1 items
// GLOBAL_FUNC_3-DAG: Pattern/ExprSpecific:               {#fn2: () -> String##() -> String#}[#() -> String#]; name=fn2: () -> String
// GLOBAL_FUNC_3: End completions
  }

  globalFunc1(fn1: { 1 }) {
    #^GLOBAL_FUNC_4^#
// GLOBAL_FUNC_4: Begin completions
// GLOBAL_FUNC_4-NOT: Decl[Struct]/OtherModule[Swift]/TypeRelation[Identical]: Int[#Int#];
// GLOBAL_FUNC_4-DAG: Pattern/ExprSpecific: {#fn2: () -> String##() -> String#}[#() -> String#];
// GLOBAL_FUNC_4-DAG: Decl[Struct]/OtherModule[Swift]/TypeRelation[Identical]: String[#String#];
// GLOBAL_FUNC_4: End completions
  }

  globalFunc1() {
    #^GLOBAL_FUNC_5^#
// Same as GLOBAL_FUNC_1
    fn2: { "" }
  }
}

struct MyStruct {
  func method1(fn1: () -> Int, fn2: () -> String) {}
}
func testMethod(value: MyStruct) {
  value.method1 {
    #^METHOD_1^#
// METHOD_1: Begin completions, 1 items
// METHOD_1-DAG: Pattern/ExprSpecific: {#fn1: () -> Int##() -> Int#}[#() -> Int#];
// METHOD_1: End completions
  }
  value.method1() {
    #^METHOD_2^#
// Same as METHOD_1
  }
  value.method1() {
    fn1: { 1 }
    #^METHOD_3^#
// METHOD_3: Begin completions, 1 items
// METHOD_3-DAG: Pattern/ExprSpecific:               {#fn2: () -> String##() -> String#}[#() -> String#]; name=fn2: () -> String
// METHOD_3: End completions
  }

  value.method1(fn1: { 1 }) {
    #^METHOD_4^#
// METHOD_4: Begin completions
// METHOD_4-NOT: Decl[Struct]/OtherModule[Swift]/TypeRelation[Identical]: Int[#Int#];
// METHOD_4-DAG: Pattern/ExprSpecific: {#fn2: () -> String##() -> String#}[#() -> String#];
// METHOD_4-DAG: Decl[Struct]/OtherModule[Swift]/TypeRelation[Identical]: String[#String#];
// METHOD_4: End completions
  }

  value.method1 {
    #^METHOD_5^#
// Same as METHOD_1
    fn2: { "" }
  }
}
