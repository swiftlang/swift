// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_1 | %FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_2 | %FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_3 | %FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_4 | %FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_5 | %FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_6 | %FileCheck %s -check-prefix=NO_RESULTS

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TYPE_IN_ENUM_CASE_DECL | %FileCheck %s -check-prefix=TYPE_IN_ENUM_CASE_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=NESTED_TYPE_IN_ENUM_CASE_DECL | %FileCheck %s -check-prefix=NESTED_TYPE_IN_ENUM_CASE_DECL

// NO_RESULTS: found code completion token

func returnsInt() -> Int {}

//===---
//===--- Test that we don't show any completion results in enum cases.
//===---

enum EnumCase1 : Int {
  case Foo = #^ENUM_CASE_1^#
}
enum EnumCase2 : Int {
  case Foo = 1 + #^ENUM_CASE_2^#
}
enum EnumCase3 : Int {
  case Foo = returnsInt()#^ENUM_CASE_3^#
}
enum EnumCase4 : Int {
  case Foo = returnsInt().#^ENUM_CASE_4^#
}
enum EnumCase5 : Int {
  case Foo = super#^ENUM_CASE_5^#
}
enum EnumCase6 : Int {
  case Foo = super.#^ENUM_CASE_6^#
}

enum EnumCase7 {
  case foo(#^TYPE_IN_ENUM_CASE_DECL^#)
}
// TYPE_IN_ENUM_CASE_DECL-DAG: Decl[Enum]/CurrModule:                   EnumCase7[#EnumCase7#];

struct Wrapper {
  struct Nested {}
}
enum EnumCase8 {
  case foo(Wrapper.#^NESTED_TYPE_IN_ENUM_CASE_DECL^#)
}
// NESTED_TYPE_IN_ENUM_CASE_DECL: Begin completions, 2 items
// NESTED_TYPE_IN_ENUM_CASE_DECL-DAG: Decl[Struct]/CurrNominal:           Nested[#Wrapper.Nested#];
// NESTED_TYPE_IN_ENUM_CASE_DECL-DAG: Keyword/None:                       Type[#Wrapper.Type#];
