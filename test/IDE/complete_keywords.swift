// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

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
// KW_DECL_STMT-DAG: Keyword[#function]/None{{(/TypeRelation\[Identical\])?}}: #function[#String#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[#file]/None{{(/TypeRelation\[Identical\])?}}: #file[#String#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[#line]/None{{(/TypeRelation\[Identical\])?}}: #line[#Int#]{{; name=.+$}}
// KW_DECL_STMT-DAG: Keyword[#column]/None{{(/TypeRelation\[Identical\])?}}: #column[#Int#]{{; name=.+$}}
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
// KW_EXPR-DAG: Keyword[#function]/None{{(/TypeRelation\[Identical\])?}}: #function[#String#]{{; name=.+$}}
// KW_EXPR-DAG: Keyword[#file]/None{{(/TypeRelation\[Identical\])?}}: #file[#String#]{{; name=.+$}}
// KW_EXPR-DAG: Keyword[#line]/None{{(/TypeRelation\[Identical\])?}}: #line[#Int#]{{; name=.+$}}
// KW_EXPR-DAG: Keyword[#column]/None{{(/TypeRelation\[Identical\])?}}: #column[#Int#]{{; name=.+$}}
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

#^TOP_LEVEL_1?check=KW_DECL_STMT;check=KW_NO_RETURN^#

for _ in 1...10 {
  #^TOP_LEVEL_2?check=KW_DECL_STMT;check=KW_NO_RETURN^#
}

if true {} #^TOP_LEVEL_AFTER_IF_1?check=KW_DECL_STMT;check=KW_NO_RETURN^#

if true {} else #^TOP_LEVEL_AFTER_IF_ELSE_1?check=AFTER_IF_ELSE^# {}

// AFTER_IF_ELSE: Begin completions, 1 items
// AFTER_IF_ELSE: Keyword[if]/None: if;

func testAfterIf1() {
  if true {} #^AFTER_IF_1?check=KW_DECL_STMT;check=KW_RETURN^#
}
func testAfterIfElse1() {
  if true {} else #^AFTER_IF_ELSE_1?check=AFTER_IF_ELSE^# {}
}

func testInFuncBody1() {
  #^IN_FUNC_BODY_1?check=KW_DECL_STMT;check=KW_RETURN^#
}

struct InStructFunc {
  func testInFuncBody2() {
    #^IN_FUNC_BODY_2?check=KW_DECL_STMT;check=KW_RETURN^#
  }
}

enum InEnumFunc {
  func testInFuncBody3() {
    #^IN_FUNC_BODY_3?check=KW_DECL_STMT;check=KW_RETURN^#
  }
}

class InClassFunc {
  func testInFuncBody4() {
    #^IN_FUNC_BODY_4?check=KW_DECL_STMT;check=KW_RETURN^#
  }
}

class InClassFunc {
  class Nested {
    func testInFuncBody5() {
      #^IN_FUNC_BODY_5?check=KW_DECL_STMT;check=KW_RETURN^#
    }
  }
}

func testInClosure1() {
  { #^IN_CLOSURE_1?check=KW_DECL_STMT;check=KW_RETURN^# }
}
func testInClosure2() {
  { #^IN_CLOSURE_2?check=KW_DECL_STMT;check=KW_RETURN^#
}
struct InVarClosureInit {
  let x = { #^IN_CLOSURE_3?check=KW_DECL_STMT;check=KW_RETURN^# }()
}

{ #^IN_CLOSURE_4?check=KW_DECL_STMT;check=KW_RETURN^# }

struct InSubscript {
  subscript(x: Int) -> Int { #^IN_SUBSCRIPT_1?check=KW_DECL_STMT;check=KW_RETURN^# }
}

struct InInit {
  init?() { #^IN_INIT_1?check=KW_DECL_STMT;check=KW_RETURN^# }
}

struct InStruct {
  #^IN_NOMINAL_DECL_1?check=KW_DECL^#
}

enum InEnum {
  #^IN_NOMINAL_DECL_2?check=KW_DECL^#
}

class InClass {
  #^IN_NOMINAL_DECL_3?check=KW_DECL^#
}

protocol InProtocol {
  #^IN_NOMINAL_DECL_4?check=KW_DECL^#
}

struct AfterOtherKeywords1 {
  public #^IN_NOMINAL_DECL_5?check=KW_DECL^#
}

struct AfterOtherKeywords2 {
  mutating #^IN_NOMINAL_DECL_6?check=KW_DECL^#
}

class AfterOtherKeywords3 {
  override #^IN_NOMINAL_DECL_7?check=KW_DECL^#
}

class AfterOtherKeywords4 {
  public override #^IN_NOMINAL_DECL_8?check=KW_DECL^#
}

extension InStruct {
  #^IN_NOMINAL_DECL_9?check=KW_DECL^#
}

extension InProtocol {
  #^IN_NOMINAL_DECL_10?check=KW_DECL^#
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
  (#^EXPR_1?check=KW_EXPR;check=KW_EXPR_NEG^#)
}
func inExpr2() {
  let x = #^EXPR_2?check=KW_EXPR;check=KW_EXPR_NEG^#
}
func inExpr3() {
  if #^EXPR_3?check=KW_EXPR;check=KW_EXPR_NEG^# {}
}
func inExpr4() {
  let x = 1
  x + #^EXPR_4?check=KW_EXPR;check=KW_EXPR_NEG^#
}
func inExpr5() {
  var x: Int
  x = #^EXPR_5?check=KW_EXPR;check=KW_EXPR_NEG^#
}
func inExpr6() -> Int {
  return #^EXPR_6?check=KW_EXPR;check=KW_EXPR_NEG^#
}

func inSwitch(val: Int) {
  switch val {
  #^SWITCH_TOP?check=KW_CASE^#
  case 1:
    foo()
  #^SWITCH_IN_CASE?check=KW_CASE^#
  }
// KW_CASE: Begin completions
// KW_CASE-DAG: Keyword[case]/None:                 case; name=case
// KW_CASE-DAG: Keyword[default]/None:              default; name=default
// KW_CASE: End completions
}
func testContextualType() {
  let _: UInt32 = #^CONTEXT_UINT32^#
// CONTEXT_UINT32: Begin completions
// CONTEXT_UINT32-DAG: Keyword[#function]/None:            #function[#String#]; name=#function
// CONTEXT_UINT32-DAG: Keyword[#file]/None:                #file[#String#]; name=#file
// CONTEXT_UINT32-DAG: Keyword[#line]/None/TypeRelation[Identical]: #line[#UInt32#]; name=#line
// CONTEXT_UINT32-DAG: Keyword[#column]/None/TypeRelation[Identical]: #column[#UInt32#]; name=#column
// CONTEXT_UINT32-DAG: Keyword[#dsohandle]/None:           #dsohandle[#UnsafeRawPointer#]; name=#dsohandle
// CONTEXT_UINT32: End completions

  let _: StaticString = #^CONTEXT_STATICSTRING^#
// CONTEXT_STATICSTRING: Begin completions
// CONTEXT_STATICSTRING-DAG: Keyword[#function]/None/TypeRelation[Identical]: #function[#StaticString#]; name=#function
// CONTEXT_STATICSTRING-DAG: Keyword[#file]/None/TypeRelation[Identical]: #file[#StaticString#]; name=#file
// CONTEXT_STATICSTRING-DAG: Keyword[#line]/None:                #line[#Int#]; name=#line
// CONTEXT_STATICSTRING-DAG: Keyword[#column]/None:              #column[#Int#]; name=#column
// CONTEXT_STATICSTRING-DAG: Keyword[#dsohandle]/None:           #dsohandle[#UnsafeRawPointer#]; name=#dsohandle
// CONTEXT_STATICSTRING: End completions
}
