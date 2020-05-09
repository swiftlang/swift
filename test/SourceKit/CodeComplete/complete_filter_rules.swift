// The top of this file is very sensitive to modification as we're using raw
// offsets for code-completion locations.

import Foo

struct TestHideName {
  func hideThis1() {}
  func hideThis2(_ x: Int, namedParam2: Int) {}
  func hideThis3(namedParam1: Int, _ unnamedParam: Int) {}
  func hideThis4(namedParam1 x: Int, namedParam2: Int) {}
  func dontHideThisByName() {}

// REQUIRES: objc_interop

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideNames.json -tok=HIDE_NAMES_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_NAMES
  func testHideName01() {
    #^HIDE_NAMES_1,hidethis^#
  }

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideNames.json -tok=HIDE_NAMES_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_NAMES
  func testHideName02() {
    self.#^HIDE_NAMES_2,hidethis^#
  }
// HIDE_NAMES-LABEL: Results for filterText: hidethis [
// HIDE_NAMES-NEXT:   dontHideThisByName()
// HIDE_NAMES-NEXT: ]
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideKeywords.json -tok=HIDE_KEYWORDS_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_LET
func testHideKeyword01() {
  #^HIDE_KEYWORDS_1^#
// HIDE_LET-NOT: {{^}}let
// HIDE_LET: var
// HIDE_LET-NOT: {{^}}let
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showKeywords.json -tok=HIDE_KEYWORDS_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_FUNC
func testHideKeyword02() {
  #^HIDE_KEYWORDS_2^#
// SHOW_FUNC-NOT: {{^}}let
// SHOW_FUNC-NOT: var
// SHOW_FUNC: func
// SHOW_FUNC-NOT: var
// SHOW_FUNC-NOT: {{^}}let
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideLiterals.json -tok=HIDE_LITERALS_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_NIL
func testHideLiteral01() {
  let x = #^HIDE_LITERALS_1^#
// HIDE_NIL-NOT: nil
// HIDE_NIL: 0
// HIDE_NIL-NOT: nil
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showLiterals.json -tok=HIDE_LITERALS_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_STRING
func testHideLiteral02() {
  let x = #^HIDE_LITERALS_2^#
// SHOW_STRING-NOT: [
// SHOW_STRING-NOT: nil
// SHOW_STRING: "abc"
// SHOW_STRING-NOT: nil
// SHOW_STRING-NOT: [
}

// FIXME: custom completions

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideModules.json -tok=HIDE_MODULES_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_FOO
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

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showSpecific.json -tok=SHOW_SPECIFIC_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_SPECIFIC_1
func testShowSpecific01() {
  #^SHOW_SPECIFIC_1,fun^#
// SHOW_SPECIFIC_1-LABEL: Results for filterText: fun [
// SHOW_SPECIFIC_1-NEXT:   func
// SHOW_SPECIFIC_1-NEXT:   myFunFunction1()
// SHOW_SPECIFIC_1-NEXT: ]
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showSpecific.json -tok=HIDE_OP_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP
func testHideOp1() {
  var local = 1
  #^HIDE_OP_1,local^#
}
// HIDE_OP-NOT: .
// HIDE_OP-NOT: =

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showSpecific.json -tok=HIDE_OP_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP -allow-empty
func testHideOp2() {
  var local = 1
  local#^HIDE_OP_2^#
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showOp.json -tok=SHOW_OP_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_OP_1
func testShowOp1() {
  var local = 1
  #^SHOW_OP_1,local^#
}
// SHOW_OP_1: local.{{$}}
// SHOW_OP_1: local={{$}}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showOp.json -tok=SHOW_OP_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_OP_2
func testShowOp2() {
  var local = 1
  local#^SHOW_OP_2^#
}
// SHOW_OP_2: {{^}}.{{$}}
// SHOW_OP_2: {{^}}={{$}}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showOp.json -tok=HIDE_OP_3 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP
func testHideOp3() {
  let local = TestHideName()
  // Implicitly hidden because we hide all the members.
  #^HIDE_OP_3,local^#
}
// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showOp.json -tok=HIDE_OP_4 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP -allow-empty
func testHideOp4() {
  let local = TestHideName()
  // Implicitly hidden because we hide all the members.
  local#^HIDE_OP_4^#
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showOp.json -tok=SHOW_OP_3 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_OP_1
func testShowOp3() {
  var local = 1
  #^SHOW_OP_3,local^#
}
// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showOp.json -tok=SHOW_OP_4 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_OP_2
func testShowOp4() {
  var local = 1
  local#^SHOW_OP_4^#
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideInnerOp.json -tok=HIDE_OP_5 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP_5 -allow-empty
func testHideOp5() {
  var local = 1
  #^HIDE_OP_5,local^#
}
// HIDE_OP_5-NOT: local.{{$}}
// HIDE_OP_5-NOT: local?.
// HIDE_OP_5-NOT: local(

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideInnerOp.json -tok=HIDE_OP_6 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP_5 -allow-empty
func testHideOp6() {
  var local: Int? = 1
  #^HIDE_OP_6,local^#
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideInnerOp.json -tok=HIDE_OP_7 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP_5 -allow-empty
func testHideOp7() {
  struct local {}
  #^HIDE_OP_7,local^#
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideInnerOp.json -tok=HIDE_OP_8 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP_8 -allow-empty
func testHideOp8() {
  var local = 1
  local#^HIDE_OP_8^#
}
// HIDE_OP_8-NOT: {{^}}.{{$}}
// HIDE_OP_8-NOT: {{^}}?.
// HIDE_OP_8-NOT: {{^}}(

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideInnerOp.json -tok=HIDE_OP_9 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP_8 -allow-empty
func testHideOp9() {
  var local: Int? = 1
  local#^HIDE_OP_9^#
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideInnerOp.json -tok=HIDE_OP_10 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_OP_8 -allow-empty
func testHideOp10() {
  struct local {}
  local#^HIDE_OP_10^#
}

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideDesc.json -tok=HIDE_DESC_1 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_DESC_1
func testHideDesc1() {
  struct Local {
    func over(_: Int) {}
    func over(_: Float) {}
  }

  Local().#^HIDE_DESC_1^#
}
// HIDE_DESC_1-NOT: over
// HIDE_DESC_1: over(Float)
// HIDE_DESC_1-NOT: over

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideDesc.json -tok=HIDE_DESC_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_DESC_2
// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/hideDesc.json -tok=HIDE_DESC_3 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=HIDE_DESC_2
func testHideDesc2() {
  struct Local {
    subscript(_: Int) -> Int { return 0 }
    subscript(_: Float) -> Float { return 0.0 }
  }

  Local()#^HIDE_DESC_2^#

  let local = Local()
  #^HIDE_DESC_3,local^#
}
// HIDE_DESC_2-NOT: [Int]
// HIDE_DESC_2: [Float]
// HIDE_DESC_2-NOT: [Int]

// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showDesc.json -tok=SHOW_DESC_2 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_DESC_2
// RUN: %complete-test -filter-rules=%S/Inputs/filter-rules/showDesc.json -tok=SHOW_DESC_3 %s -- -F %S/../Inputs/libIDE-mock-sdk | %FileCheck %s -check-prefix=SHOW_DESC_2
func testHideDesc2() {
  struct Local {
    subscript(_: Int) -> Int { return 0 }
    subscript(_: Float) -> Float { return 0.0 }
  }

  Local()#^SHOW_DESC_2^#

  let local = Local()
  #^SHOW_DESC_3,local^#
}
// SHOW_DESC_2-NOT: [Int]
// SHOW_DESC_2: [Float]
// SHOW_DESC_2-NOT: [Int]
