// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_2 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_2 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_3 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_4 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_WHILE_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_WHILE_2 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_DO_WHILE_1 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INIT_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INIT_2 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INIT_3 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_2 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_3 | FileCheck %s -check-prefix=COND_COMMON

// FIXME: should have 'i' in these results.
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_I_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_I_2 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_2 | FileCheck %s -check-prefix=COND_COMMON

// FIXME: should have 'i' in these results.
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_2 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_3 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_4 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_EACH_EXPR_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_EACH_EXPR_2 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_EXPR_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_EXPR_2 | FileCheck %s -check-prefix=COND_COMMON

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_2 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_3 | FileCheck %s -check-prefix=COND_COMMON

// FIXME: should have 'i' in these results.
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_I_1 | FileCheck %s -check-prefix=COND_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_I_2 | FileCheck %s -check-prefix=COND_COMMON

// FIXME: should have 'i' and 'j' in these results.
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_I_J_1 | FileCheck %s -check-prefix=COND_COMMON

struct FooStruct {
  var instanceVar : Int
}

func testIf1(fooObject: FooStruct) {
  if #^COND_IF_1^#
}

func testIf2(fooObject: FooStruct) {
  if #^COND_IF_2^# {
  }
}

func testIfElseIf1(fooObject: FooStruct) {
  if true {
  } else if #^COND_IF_ELSE_IF_1^#
}

func testIfElseIf2(fooObject: FooStruct) {
  if true {
  } else if #^COND_IF_ELSE_IF_2^# {
  }
}

func testIfElseIf3(fooObject: FooStruct) {
  if true {
  } else if true {
  } else if #^COND_IF_ELSE_IF_3^#
}

func testIfElseIf4(fooObject: FooStruct) {
  if true {
  } else if true {
  } else if #^COND_IF_ELSE_IF_4^# {
  }
}

func testWhile1(fooObject: FooStruct) {
  while #^COND_WHILE_1^#
}

func testWhile2(fooObject: FooStruct) {
  while #^COND_WHILE_2^# {
  }
}

func testDoWhile1(fooObject: FooStruct) {
  do {
  } while #^COND_DO_WHILE_1^#
}

func testCStyleForInit1(fooObject: FooStruct) {
  for #^C_STYLE_FOR_INIT_1^#
}

func testCStyleForInit2(fooObject: FooStruct) {
  for #^C_STYLE_FOR_INIT_2^#;
}

func testCStyleForInit3(fooObject: FooStruct) {
  for #^C_STYLE_FOR_INIT_3^# ;
}

func testCStyleForCond1(fooObject: FooStruct) {
  for ; #^C_STYLE_FOR_COND_1^#
}

func testCStyleForCond2(fooObject: FooStruct) {
  for ; #^C_STYLE_FOR_COND_2^#;
}

func testCStyleForCond3(fooObject: FooStruct) {
  for ; #^C_STYLE_FOR_COND_3^# ;
}

func testCStyleForCondI1(fooObject: FooStruct) {
  for var i = 0; #^C_STYLE_FOR_COND_I_1^#
}

func testCStyleForCondI2(fooObject: FooStruct) {
  for var i = unknown_var; #^C_STYLE_FOR_COND_I_2^#
}

func testCStyleForIncr1(fooObject: FooStruct) {
  for ; ; #^C_STYLE_FOR_INCR_1^#
}

func testCStyleForIncr2(fooObject: FooStruct) {
  for ; ; #^C_STYLE_FOR_INCR_2^# {
  }
}

func testCStyleForIncrI1(fooObject: FooStruct) {
  for var i = 0; true; #^C_STYLE_FOR_INCR_I_1^#
}

func testCStyleForIncrI2(fooObject: FooStruct) {
  for var i = 0; i != 10; #^C_STYLE_FOR_INCR_I_2^#
}

func testCStyleForIncrI3(fooObject: FooStruct) {
  for var i = 0; unknown_var != 10; #^C_STYLE_FOR_INCR_I_3^#
}

func testCStyleForIncrI4(fooObject: FooStruct) {
  for var i = unknown_var; unknown_var != 10; #^C_STYLE_FOR_INCR_I_4^#
}

func testForEachExpr1(fooObject: FooStruct) {
  for i in #^FOR_EACH_EXPR_1^#
}

func testForEachExpr2(fooObject: FooStruct) {
  for i in #^FOR_EACH_EXPR_2^# {
  }
}

func testSwitchExpr1(fooObject: FooStruct) {
  switch #^SWITCH_EXPR_1^#
}

func testSwitchExpr2(fooObject: FooStruct) {
  switch #^SWITCH_EXPR_2^# {
  }
}

func testSwitchCaseWhereExpr1(fooObject: FooStruct) {
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_1^#
  }
}

func testSwitchCaseWhereExpr2(fooObject: FooStruct) {
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_2^#:
  }
}

func testSwitchCaseWhereExpr3(fooObject: FooStruct) {
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_3^# :
  }
}

func testSwitchCaseWhereExprI1(fooObject: FooStruct) {
  switch (0, 42) {
    case (var i, 0) where #^SWITCH_CASE_WHERE_EXPR_I_1^#
  }
}

func testSwitchCaseWhereExprI2(fooObject: FooStruct) {
  switch (0, 42) {
    case (0, var i) where #^SWITCH_CASE_WHERE_EXPR_I_2^#
  }
}

func testSwitchCaseWhereExprIJ1(fooObject: FooStruct) {
  switch (0, 42) {
    case (var i, var j) where #^SWITCH_CASE_WHERE_EXPR_I_J_1^#
  }
}

// COND_COMMON: Begin completions
// COND_COMMON-DAG: SwiftDecl: true[#Bool#]{{$}}
// COND_COMMON-DAG: SwiftDecl: false[#Bool#]{{$}}
// COND_COMMON-DAG: SwiftDecl: fooObject[#FooStruct#]{{$}}
// COND_COMMON-DAG: SwiftDecl: FooStruct[#FooStruct.metatype#]{{$}}
// COND_COMMON: End completions

