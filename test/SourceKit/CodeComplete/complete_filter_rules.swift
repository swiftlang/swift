// The top of this file is very sensitive to modification as we're using raw
// offsets for code-completion locations.

import Foo

struct TestHideName {
  func hideThis1() {}
  func hideThis2(_ x: Int, namedParam2: Int) {}
  func hideThis3(namedParam1: Int, _ unnamedParam: Int) {}
  func hideThis4(namedParam1 x: Int, namedParam2: Int) {}
  func dontHideThisByName() {}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideNames.json -tok=HIDE_NAMES_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=HIDE_NAMES
  func testHideName01() {
    #^HIDE_NAMES_1,hidethis^#
  }

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideNames.json -tok=HIDE_NAMES_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=HIDE_NAMES
  func testHideName02() {
    self.#^HIDE_NAMES_2,hidethis^#
  }
// HIDE_NAMES-LABEL: Results for filterText: hidethis [
// HIDE_NAMES-NEXT:   dontHideThisByName()
// HIDE_NAMES-NEXT: ]
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideKeywords.json -tok=HIDE_KEYWORDS_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=HIDE_LET
func testHideKeyword01() {
  #^HIDE_KEYWORDS_1^#
// HIDE_LET-NOT: let
// HIDE_LET: var
// HIDE_LET-NOT: let
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showKeywords.json -tok=HIDE_KEYWORDS_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=SHOW_FUNC
func testHideKeyword02() {
  #^HIDE_KEYWORDS_2^#
// SHOW_FUNC-NOT: let
// SHOW_FUNC-NOT: var
// SHOW_FUNC: func
// SHOW_FUNC-NOT: var
// SHOW_FUNC-NOT: let
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideLiterals.json -tok=HIDE_LITERALS_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=HIDE_NIL
func testHideLiteral01() {
  let x = #^HIDE_LITERALS_1^#
// HIDE_NIL-NOT: nil
// HIDE_NIL: 0
// HIDE_NIL-NOT: nil
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showLiterals.json -tok=HIDE_LITERALS_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=SHOW_STRING
func testHideLiteral02() {
  let x = #^HIDE_LITERALS_2^#
// SHOW_STRING-NOT: [
// SHOW_STRING-NOT: nil
// SHOW_STRING: "abc"
// SHOW_STRING-NOT: nil
// SHOW_STRING-NOT: [
}

// FIXME: custom completions

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideModules.json -tok=HIDE_MODULES_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=HIDE_FOO
func testHideModule01() {
  let x: #^HIDE_MODULES_1^#
// FIXME: submodules
// HIDE_FOO-NOT: FooStruct1
// HIDE_FOO-NOT: FooClassBase
// HIDE_FOO: FooStruct2
// HIDE_FOO: FooHelperSubEnum1
// HIDE_FOO-NOT: FooStruct1
// HIDE_FOO-NOT: FooClassBase
}

func myFunFunction1() {}
func myFunFunction2() {}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showSpecific.json -tok=SHOW_SPECIFIC_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | FileCheck %s -check-prefix=SHOW_SPECIFIC_1
func testShowSpecific01() {
  #^SHOW_SPECIFIC_1,fun^#
// SHOW_SPECIFIC_1-LABEL: Results for filterText: fun [
// SHOW_SPECIFIC_1-NEXT:   func
// SHOW_SPECIFIC_1-NEXT:   myFunFunction1()
// SHOW_SPECIFIC_1-NEXT: ]
}
