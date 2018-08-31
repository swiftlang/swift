// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_1 > %t.top1
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.top1
// RUN: %FileCheck %s -check-prefix=KW_NO_RETURN < %t.top1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_2 > %t.top2
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.top2
// RUN: %FileCheck %s -check-prefix=KW_NO_RETURN < %t.top2

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_AFTER_IF_1 > %t.top3
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.top3
// RUN: %FileCheck %s -check-prefix=KW_NO_RETURN < %t.top3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_AFTER_IF_ELSE_1 | %FileCheck %s -check-prefix=AFTER_IF_ELSE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AFTER_IF_1 > %t.if1
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.if1
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.if1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=AFTER_IF_ELSE_1 | %FileCheck %s -check-prefix=AFTER_IF_ELSE

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_1 > %t.func1
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.func1
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.func1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_2 > %t.func2
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.func2
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.func2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_3 > %t.func3
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.func3
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.func3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_4 > %t.func4
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.func4
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.func4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_FUNC_BODY_5 > %t.func5
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.func5
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.func5

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_1 > %t.clos1
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.clos1
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.clos1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_2 > %t.clos2
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.clos2
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.clos2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_3 > %t.clos3
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.clos3
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.clos3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_CLOSURE_4 > %t.clos4
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.clos4
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.clos4

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_SUBSCRIPT_1 > %t.subs
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.subs
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.subs

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_INIT_1 > %t.init
// RUN: %FileCheck %s -check-prefix=KW_DECL_STMT < %t.init
// RUN: %FileCheck %s -check-prefix=KW_RETURN < %t.init

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_1 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_2 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_3 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_4 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_5 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_6 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_7 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_8 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_9 | %FileCheck %s -check-prefix=KW_DECL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=IN_NOMINAL_DECL_10 | %FileCheck %s -check-prefix=KW_DECL

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_KEYWORD0 | %FileCheck %s -check-prefix=SUPER_KEYWORD0
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_KEYWORD1 | %FileCheck %s -check-prefix=SUPER_KEYWORD1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_KEYWORD2 | %FileCheck %s -check-prefix=SUPER_KEYWORD2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUPER_KEYWORD3 | %FileCheck %s -check-prefix=SUPER_KEYWORD3

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_1 > %t.expr1
// RUN: %FileCheck %s -check-prefix=KW_EXPR < %t.expr1
// RUN: %FileCheck %s -check-prefix=KW_EXPR_NEG < %t.expr1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_2 > %t.expr2
// RUN: %FileCheck %s -check-prefix=KW_EXPR < %t.expr2
// RUN: %FileCheck %s -check-prefix=KW_EXPR_NEG < %t.expr2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_3 > %t.expr3
// RUN: %FileCheck %s -check-prefix=KW_EXPR < %t.expr3
// RUN: %FileCheck %s -check-prefix=KW_EXPR_NEG < %t.expr3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_4 > %t.expr4
// RUN: %FileCheck %s -check-prefix=KW_EXPR < %t.expr4
// RUN: %FileCheck %s -check-prefix=KW_EXPR_NEG < %t.expr4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_5 > %t.expr5
// RUN: %FileCheck %s -check-prefix=KW_EXPR < %t.expr5
// RUN: %FileCheck %s -check-prefix=KW_EXPR_NEG < %t.expr5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_6 > %t.expr6
// RUN: %FileCheck %s -check-prefix=KW_EXPR < %t.expr6
// RUN: %FileCheck %s -check-prefix=KW_EXPR_NEG < %t.expr6

// KW_RETURN: Keyword[return]/None: return{{; name=.+$}}
// KW_NO_RETURN-NOT: Keyword[return]

// KW_DECL: Begin completions
// KW_DECL-DAG: Keyword[class]/None: class{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: convenience{{; name=.+$}}
// KW_DECL-DAG: Keyword[deinit]/None: deinit{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: dynamic{{; name=.+$}}
// KW_DECL-DAG: Keyword[enum]/None: enum{{; name=.+$}}
// KW_DECL-DAG: Keyword[extension]/None: extension{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: final{{; name=.+$}}
// KW_DECL-DAG: Keyword[func]/None: func{{; name=.+$}}
// KW_DECL-DAG: Keyword[import]/None: import{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: infix{{; name=.+$}}
// KW_DECL-DAG: Keyword[init]/None: init{{; name=.+$}}
// KW_DECL-DAG: Keyword[internal]/None: internal{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: lazy{{; name=.+$}}
// KW_DECL-DAG: Keyword[let]/None: let{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: mutating{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: nonmutating{{; name=.+$}}
// KW_DECL-DAG: Keyword[operator]/None: operator{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: optional{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: override{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: postfix{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: prefix{{; name=.+$}}
// KW_DECL-DAG: Keyword[private]/None: private{{; name=.+$}}
// KW_DECL-DAG: Keyword[protocol]/None: protocol{{; name=.+$}}
// KW_DECL-DAG: Keyword[public]/None: public{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: required{{; name=.+$}}
// KW_DECL-DAG: Keyword[static]/None: static{{; name=.+$}}
// KW_DECL-DAG: Keyword[struct]/None: struct{{; name=.+$}}
// KW_DECL-DAG: Keyword[subscript]/None: subscript{{; name=.+$}}
// KW_DECL-DAG: Keyword[typealias]/None: typealias{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: unowned{{; name=.+$}}
// KW_DECL-DAG: Keyword[var]/None: var{{; name=.+$}}
// KW_DECL-DAG: Keyword/None: weak{{; name=.+$}}
// KW_DECL: End completions

// KW_DECL_STMT: Begin completions
//
// Declaration keywords.
//
// KW_DECL_STMT-DAG: Keyword[class]/None: class{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: convenience{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[deinit]/None: deinit{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: dynamic{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[enum]/None: enum{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[extension]/None: extension{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: final{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[func]/None: func{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[import]/None: import{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: infix{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[init]/None: init{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[internal]/None: internal{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: lazy{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[let]/None: let{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: mutating{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: nonmutating{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[operator]/None: operator{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: optional{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: override{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: postfix{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: prefix{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[private]/None: private{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[protocol]/None: protocol{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[public]/None: public{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: required{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[static]/None: static{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[struct]/None: struct{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[subscript]/None: subscript{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[typealias]/None: typealias{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: unowned{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[var]/None: var{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword/None: weak{{; name=.+$}}
//
// Statement keywords.
//
// KW_DECL_STMT-DAG: Keyword[if]/None: if{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[do]/None: do{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[else]/None: else{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[for]/None: for{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[in]/None: in{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[while]/None: while{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[break]/None: break{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[continue]/None: continue{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[fallthrough]/None: fallthrough{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[switch]/None: switch{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[case]/None: case{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[default]/None: default{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[where]/None: where{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[catch]/None: catch{{; name=.+$}}
//
// Misc.
//
// KW_DECL_STMT-DAG: Keyword[throw]/None: throw{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[try]/None: try{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[try]/None: try!{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[try]/None: try?{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[#function]/None: #function[#String#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[#file]/None: #file[#String#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[#line]/None: #line[#Int#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[#column]/None: #column[#Int#]{{; name=.+$}}
//
// Literals
//
// KW_DECL_STMT-DAG: Literal[Boolean]/None: false[#Bool#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Literal[Boolean]/None: true[#Bool#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Literal[Nil]/None: nil{{; name=.+$}}
// KW_DECL_STMT: End completions


// KW_EXPR: Begin completions
//
// Expressions
//
// KW_EXPR-DAG: Keyword[try]/None: try{{; name=.+$}}
// KW_EXPR-DAG: Keyword[try]/None: try!{{; name=.+$}}
// KW_EXPR-DAG: Keyword[try]/None: try?{{; name=.+$}}
// KW_EXPR-DAG: Keyword[#function]/None: #function[#String#]{{; name=.+$}}
// KW_EXPR-DAG: Keyword[#file]/None: #file[#String#]{{; name=.+$}}
// KW_EXPR-DAG: Keyword[#line]/None: #line[#Int#]{{; name=.+$}}
// KW_EXPR-DAG: Keyword[#column]/None: #column[#Int#]{{; name=.+$}}
//
// let and var
//
// KW_EXPR-DAG: Keyword[let]/None: let{{; name=.+$}}
// KW_EXPR-DAG: Keyword[var]/None: var{{; name=.+$}}
//
// Literals
//
// KW_EXPR-DAG: Literal[Boolean]/None{{(/TypeRelation\[Identical\])?}}: false[#Bool#]{{; name=.+$}}
// KW_EXPR-DAG: Literal[Boolean]/None{{(/TypeRelation\[Identical\])?}}: true[#Bool#]{{; name=.+$}}
// KW_EXPR-DAG: Literal[Nil]/None: nil{{; name=.+$}}
// KW_EXPR: End completions

// KW_EXPR_NEG: Begin completions
//
// Declaration keywords
//
// KW_EXPR_NEG-NOT: Keyword{{.*}}class
// KW_EXPR_NEG-NOT: Keyword{{.*}}convenience
// KW_EXPR_NEG-NOT: Keyword{{.*}}mutating
// KW_EXPR_NEG-NOT: Keyword{{.*}}weak
//
// Statement keywords
//
// KW_EXPR_NEG-NOT: Keyword{{.*}}throw
// KW_EXPR_NEG-NOT: Keyword{{.*}}while
// KW_EXPR_NEG-NOT: Keyword{{.*}}switch
// KW_EXPR_NEG-NOT: Keyword{{.*}}catch
// KW_EXPR_NEG-NOT: Keyword{{.*}}break
// KW_EXPR_NEG: End completions

#^TOP_LEVEL_1^#

for _ in 1...10 {
  #^TOP_LEVEL_2^#
}

if true {} #^TOP_LEVEL_AFTER_IF_1^#

if true {} else #^TOP_LEVEL_AFTER_IF_ELSE_1^# {}

// AFTER_IF_ELSE: Begin completions, 1 items
// AFTER_IF_ELSE: Keyword[if]/None: if;

func testAfterIf1() {
  if true {} #^AFTER_IF_1^#
}
func testAfterIfElse1() {
  if true {} else #^AFTER_IF_ELSE_1^# {}
}

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

func testInClosure1() {
  { #^IN_CLOSURE_1^# }
}
func testInClosure2() {
  { #^IN_CLOSURE_2^#
}
struct InVarClosureInit {
  let x = { #^IN_CLOSURE_3^# }()
}

{ #^IN_CLOSURE_4^# }

struct InSubscript {
  subscript(x: Int) -> Int { #^IN_SUBSCRIPT_1^# }
}

struct InInit {
  init?() { #^IN_INIT_1^# }
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

class SuperSuperClass {
   func f1() {
    #^SUPER_KEYWORD0^#
// SUPER_KEYWORD0-NOT: Keyword{{.*}}super
  }
}

class SuperClass : SuperSuperClass {
   func f2() {
    #^SUPER_KEYWORD1^#
  }
// SUPER_KEYWORD1: Keyword[super]/CurrNominal:                       super[#SuperSuperClass#]; name=super{{$}}
}

class SubClass : SuperClass {
  func f3() {
    #^SUPER_KEYWORD2^#
  }
// SUPER_KEYWORD2: Keyword[super]/CurrNominal:                       super[#SuperClass#]; name=super{{$}}
}

extension SubClass {
  func f4() {
    #^SUPER_KEYWORD3^#
  }
// SUPER_KEYWORD3: Keyword[super]/CurrNominal:                       super[#SuperClass#]; name=super{{$}}
}

func inExpr1() {
  (#^EXPR_1^#)
}
func inExpr2() {
  let x = #^EXPR_2^#
}
func inExpr3() {
  if #^EXPR_3^# {}
}
func inExpr4() {
  let x = 1
  x + #^EXPR_4^#
}
func inExpr5() {
  var x: Int
  x = #^EXPR_5^#
}
func inExpr6() -> Int {
  return #^EXPR_6^#
}
