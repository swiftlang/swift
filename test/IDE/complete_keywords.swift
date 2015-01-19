// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_1 | FileCheck %s -check-prefix=KW_DECL_STMT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_1 | FileCheck %s -check-prefix=KW_DECL_STMT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_2 | FileCheck %s -check-prefix=KW_DECL_STMT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_3 | FileCheck %s -check-prefix=KW_DECL_STMT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_4 | FileCheck %s -check-prefix=KW_DECL_STMT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_5 | FileCheck %s -check-prefix=KW_DECL_STMT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_1 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_2 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_3 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_4 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_5 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_6 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_7 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_8 | FileCheck %s -check-prefix=KW_DECL

// KW_DECL: Begin completions
// KW_DECL-DAG: Keyword/None: class{{$}}
// KW_DECL-DAG: Keyword/None: convenience{{$}}
// KW_DECL-DAG: Keyword/None: deinit{{$}}
// KW_DECL-DAG: Keyword/None: dynamic{{$}}
// KW_DECL-DAG: Keyword/None: enum{{$}}
// KW_DECL-DAG: Keyword/None: extension{{$}}
// KW_DECL-DAG: Keyword/None: final{{$}}
// KW_DECL-DAG: Keyword/None: func{{$}}
// KW_DECL-DAG: Keyword/None: import{{$}}
// KW_DECL-DAG: Keyword/None: infix{{$}}
// KW_DECL-DAG: Keyword/None: init{{$}}
// KW_DECL-DAG: Keyword/None: internal{{$}}
// KW_DECL-DAG: Keyword/None: lazy{{$}}
// KW_DECL-DAG: Keyword/None: let{{$}}
// KW_DECL-DAG: Keyword/None: mutating{{$}}
// KW_DECL-DAG: Keyword/None: nonmutating{{$}}
// KW_DECL-DAG: Keyword/None: operator{{$}}
// KW_DECL-DAG: Keyword/None: optional{{$}}
// KW_DECL-DAG: Keyword/None: override{{$}}
// KW_DECL-DAG: Keyword/None: postfix{{$}}
// KW_DECL-DAG: Keyword/None: prefix{{$}}
// KW_DECL-DAG: Keyword/None: private{{$}}
// KW_DECL-DAG: Keyword/None: protocol{{$}}
// KW_DECL-DAG: Keyword/None: public{{$}}
// KW_DECL-DAG: Keyword/None: required{{$}}
// KW_DECL-DAG: Keyword/None: static{{$}}
// KW_DECL-DAG: Keyword/None: struct{{$}}
// KW_DECL-DAG: Keyword/None: subscript{{$}}
// KW_DECL-DAG: Keyword/None: typealias{{$}}
// KW_DECL-DAG: Keyword/None: unowned{{$}}
// KW_DECL-DAG: Keyword/None: var{{$}}
// KW_DECL-DAG: Keyword/None: weak{{$}}
// KW_DECL: End completions

// KW_DECL_STMT: Begin completions
//
// Declaration keywords.
//
// KW_DECL_STMT-DAG: Keyword/None: class{{$}}
// KW_DECL_STMT-DAG: Keyword/None: convenience{{$}}
// KW_DECL_STMT-DAG: Keyword/None: deinit{{$}}
// KW_DECL_STMT-DAG: Keyword/None: dynamic{{$}}
// KW_DECL_STMT-DAG: Keyword/None: enum{{$}}
// KW_DECL_STMT-DAG: Keyword/None: extension{{$}}
// KW_DECL_STMT-DAG: Keyword/None: final{{$}}
// KW_DECL_STMT-DAG: Keyword/None: func{{$}}
// KW_DECL_STMT-DAG: Keyword/None: import{{$}}
// KW_DECL_STMT-DAG: Keyword/None: infix{{$}}
// KW_DECL_STMT-DAG: Keyword/None: init{{$}}
// KW_DECL_STMT-DAG: Keyword/None: internal{{$}}
// KW_DECL_STMT-DAG: Keyword/None: lazy{{$}}
// KW_DECL_STMT-DAG: Keyword/None: let{{$}}
// KW_DECL_STMT-DAG: Keyword/None: mutating{{$}}
// KW_DECL_STMT-DAG: Keyword/None: nonmutating{{$}}
// KW_DECL_STMT-DAG: Keyword/None: operator{{$}}
// KW_DECL_STMT-DAG: Keyword/None: optional{{$}}
// KW_DECL_STMT-DAG: Keyword/None: override{{$}}
// KW_DECL_STMT-DAG: Keyword/None: postfix{{$}}
// KW_DECL_STMT-DAG: Keyword/None: prefix{{$}}
// KW_DECL_STMT-DAG: Keyword/None: private{{$}}
// KW_DECL_STMT-DAG: Keyword/None: protocol{{$}}
// KW_DECL_STMT-DAG: Keyword/None: public{{$}}
// KW_DECL_STMT-DAG: Keyword/None: required{{$}}
// KW_DECL_STMT-DAG: Keyword/None: static{{$}}
// KW_DECL_STMT-DAG: Keyword/None: struct{{$}}
// KW_DECL_STMT-DAG: Keyword/None: subscript{{$}}
// KW_DECL_STMT-DAG: Keyword/None: typealias{{$}}
// KW_DECL_STMT-DAG: Keyword/None: unowned{{$}}
// KW_DECL_STMT-DAG: Keyword/None: var{{$}}
// KW_DECL_STMT-DAG: Keyword/None: weak{{$}}
//
// Statement keywords.
//
// KW_DECL_STMT-DAG: Keyword/None: if{{$}}
// KW_DECL_STMT-DAG: Keyword/None: do{{$}}
// KW_DECL_STMT-DAG: Keyword/None: else{{$}}
// KW_DECL_STMT-DAG: Keyword/None: for{{$}}
// KW_DECL_STMT-DAG: Keyword/None: in{{$}}
// KW_DECL_STMT-DAG: Keyword/None: while{{$}}
// KW_DECL_STMT-DAG: Keyword/None: return{{$}}
// KW_DECL_STMT-DAG: Keyword/None: break{{$}}
// KW_DECL_STMT-DAG: Keyword/None: continue{{$}}
// KW_DECL_STMT-DAG: Keyword/None: fallthrough{{$}}
// KW_DECL_STMT-DAG: Keyword/None: switch{{$}}
// KW_DECL_STMT-DAG: Keyword/None: case{{$}}
// KW_DECL_STMT-DAG: Keyword/None: default{{$}}
// KW_DECL_STMT-DAG: Keyword/None: where{{$}}
//
// Misc.
//
// KW_DECL_STMT-DAG: Keyword/None: __FUNCTION__[#String#]{{$}}
// KW_DECL_STMT-DAG: Keyword/None: __FILE__[#String#]{{$}}
// KW_DECL_STMT-DAG: Keyword/None: __LINE__[#Int#]{{$}}
// KW_DECL_STMT-DAG: Keyword/None: __COLUMN__[#Int#]{{$}}
// KW_DECL_STMT-DAG: Keyword/None: false[#Bool#]{{$}}
// KW_DECL_STMT-DAG: Keyword/None: true[#Bool#]{{$}}
// KW_DECL_STMT-DAG: Keyword/None: nil{{$}}
// KW_DECL_STMT: End completions

#^TOP_LEVEL_1^#

func testInFuncBody1() {
  #^IN_FUNC_BODY_1^#
}

struct InStructFunc {
  func testInFuncBody2() {
    #^IN_FUNC_BODY_2^#
  }
}

enum InEnumFunc {
  func testInFuncBody3() {
    #^IN_FUNC_BODY_3^#
  }
}

class InClassFunc {
  func testInFuncBody4() {
    #^IN_FUNC_BODY_4^#
  }
}

class InClassFunc {
  class Nested {
    func testInFuncBody5() {
      #^IN_FUNC_BODY_5^#
    }
  }
}


struct InStruct {
  #^IN_NOMINAL_DECL_1^#
}

enum InEnum {
  #^IN_NOMINAL_DECL_2^#
}

class InClass {
  #^IN_NOMINAL_DECL_3^#
}

protocol InProtocol {
  #^IN_NOMINAL_DECL_4^#
}

struct AfterOtherKeywords1 {
  public #^IN_NOMINAL_DECL_5^#
}

struct AfterOtherKeywords2 {
  mutating #^IN_NOMINAL_DECL_6^#
}

class AfterOtherKeywords3 {
  override #^IN_NOMINAL_DECL_7^#
}

class AfterOtherKeywords4 {
  public override #^IN_NOMINAL_DECL_8^#
}

