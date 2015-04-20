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
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_9 | FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_10 | FileCheck %s -check-prefix=KW_DECL

// KW_DECL: Begin completions
// KW_DECL-DAG: Keyword/None: class{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: convenience{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: deinit{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: dynamic{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: enum{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: extension{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: final{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: func{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: import{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: infix{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: init{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: internal{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: lazy{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: let{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: mutating{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: nonmutating{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: operator{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: optional{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: override{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: postfix{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: prefix{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: private{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: protocol{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: public{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: required{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: static{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: struct{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: subscript{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: typealias{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: unowned{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: var{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: weak{{; name=.+$}}
// KW_DECL: End completions

// KW_DECL_STMT: Begin completions
//
// Declaration keywords.
//
// KW_DECL_STMT-DAG: Keyword/None: class{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: convenience{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: deinit{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: dynamic{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: enum{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: extension{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: final{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: func{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: import{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: infix{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: init{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: internal{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: lazy{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: let{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: mutating{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: nonmutating{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: operator{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: optional{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: override{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: postfix{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: prefix{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: private{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: protocol{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: public{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: required{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: static{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: struct{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: subscript{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: typealias{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: unowned{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: var{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: weak{{; name=.+$}}
//
// Statement keywords.
//
// KW_DECL_STMT-DAG: Keyword/None: if{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: do{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: else{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: for{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: in{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: while{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: return{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: break{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: continue{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: fallthrough{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: switch{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: case{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: default{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: where{{; name=.+$}}
//
// Misc.
//
// KW_DECL_STMT-DAG: Keyword/None: __FUNCTION__[#String#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: __FILE__[#String#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: __LINE__[#Int#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: __COLUMN__[#Int#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: false[#Bool#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: true[#Bool#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: nil{{; name=.+$}}
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

extension InStruct {
  #^IN_NOMINAL_DECL_9^#
}

extension InProtocol {
  #^IN_NOMINAL_DECL_10^#
}
