// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_1 | FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_2 | FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_3 | FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_4 | FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_5 | FileCheck %s -check-prefix=NO_RESULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=ENUM_CASE_6 | FileCheck %s -check-prefix=NO_RESULTS

// NO_RESULTS: found code completion token
// NO_RESULTS-NOT: Begin completions

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

