// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_IN_FUNC_PARAM | %FileCheck %s -check-prefix=ANY_IN_FUNC_PARAM
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_IN_VAR_TYPE | %FileCheck %s -check-prefix=ANY_IN_VAR_TYPE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_METATYPE_VARIABLE | %FileCheck %s -check-prefix=ANY_METATYPE_VARIABLE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_METATYPE_MEMBER | %FileCheck %s -check-prefix=ANY_METATYPE_MEMBER
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ANY_IN_TYPEALIAS | %FileCheck %s -check-prefix=ANY_IN_TYPEALIAS

func testAnyInParamList(a: #^ANY_IN_FUNC_PARAM^#
// ANY_IN_FUNC_PARAM: Begin completions
// ANY_IN_FUNC_PARAM-DAG: Keyword/None: Any[#Any#]; name=Any
// ANY_IN_FUNC_PARAM: End completions

func scope() {
  var a: #^ANY_IN_VAR_TYPE^#
  // ANY_IN_VAR_TYPE: Begin completions
  // ANY_IN_VAR_TYPE-DAG: Keyword/None: Any[#Any#]; name=Any
  // ANY_IN_VAR_TYPE: End completions
}

let _: Any.Type = #^ANY_METATYPE_VARIABLE^#
// ANY_METATYPE_VARIABLE: Begin completions
// ANY_METATYPE_VARIABLE-DAG: Keyword/None: Any[#Any#]; name=Any
// ANY_METATYPE_VARIABLE: End completions

_ = Int.#^ANY_METATYPE_MEMBER^#
// ANY_METATYPE_MEMBER: Begin completions
// ANY_METATYPE_MEMBER-NOT: Any
// ANY_METATYPE_MEMBER: End completions

typealias A = #^ANY_IN_TYPEALIAS^#
// ANY_IN_TYPEALIAS: Begin completions
// ANY_IN_TYPEALIAS-DAG: Keyword/None: Any[#Any#]; name=Any
// ANY_IN_TYPEALIAS: End completions


