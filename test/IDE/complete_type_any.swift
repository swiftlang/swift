// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

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

func testRdar64812321() {
  func foo<T>(x: T) {}
  func foo(x: Any.Type) {}

  struct MyStruct {}
  let myStruct = MyStruct()

  foo(x: #^ANY_RELATIONSHIP^#)
  // The MyStruct type should not be preferred over the myStruct instance.

// ANY_RELATIONSHIP: Begin completions
// ANY_RELATIONSHIP-DAG: Decl[LocalVar]/Local:              myStruct[#MyStruct#]; name=myStruct
// ANY_RELATIONSHIP-DAG: Decl[Struct]/Local:            MyStruct[#MyStruct#]; name=MyStruct
// ANY_RELATIONSHIP: End completions
}

func testRdar84684686() {
  func foo(_ x: Any?) {}

  struct S {
    static func bar(x: Int) -> Int { x }
  }

  // We should suggest a function call to `bar` here (i.e. `bar(x: <#Int#>)`), not a function reference (i.e. `bar(x:)`)
  foo(S.#^ANY_PREFERS_FUNCTION_CALL^#)
// ANY_PREFERS_FUNCTION_CALL: Begin completions
// ANY_PREFERS_FUNCTION_CALL-DAG: Decl[StaticMethod]/CurrNominal:     bar({#x: Int#})[#Int#]; name=bar(x:)
// ANY_PREFERS_FUNCTION_CALL: End completions
}
