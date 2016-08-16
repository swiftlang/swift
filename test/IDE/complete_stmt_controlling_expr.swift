// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_2 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_3 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_4 | %FileCheck %s -check-prefix=COND_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_2 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_3 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_4 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_5 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_IF_ELSE_IF_6 | %FileCheck %s -check-prefix=COND_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_WHILE_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_WHILE_2 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_WHILE_3 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_WHILE_4 | %FileCheck %s -check-prefix=COND_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_DO_WHILE_1 | %FileCheck %s -check-prefix=COND-WITH-RELATION
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COND_DO_WHILE_2 | %FileCheck %s -check-prefix=COND-WITH-RELATION1

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INIT_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INIT_2 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INIT_3 | %FileCheck %s -check-prefix=COND_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_2 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_3 | %FileCheck %s -check-prefix=COND_COMMON


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_I_1 > %t.cond.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.cond.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_EXPR_SPECIFIC < %t.cond.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_I_2 > %t.cond.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.cond.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_ERROR_EXPR_SPECIFIC < %t.cond.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_COND_I_E_1 > %t.cond.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.cond.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_E_EXPR_SPECIFIC < %t.cond.txt



// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_2 | %FileCheck %s -check-prefix=COND_COMMON


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_1 > %t.incr.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.incr.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_EXPR_SPECIFIC < %t.incr.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_2 > %t.incr.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.incr.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_EXPR_SPECIFIC < %t.incr.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_3 > %t.incr.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.incr.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_EXPR_SPECIFIC < %t.incr.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_4 > %t.incr.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.incr.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_ERROR_EXPR_SPECIFIC < %t.incr.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_INCR_I_E_1 > %t.incr.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.incr.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_E_EXPR_SPECIFIC < %t.incr.txt


// FIXME: should have 'i' in these results.
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_BODY_I_1 > %t.body.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.body.txt
// FIXME: %FileCheck %s -check-prefix=WITH_I_INT_EXPR_SPECIFIC < %t.body.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_BODY_I_2 > %t.body.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.body.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_EXPR_SPECIFIC < %t.body.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_BODY_I_3 > %t.body.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.body.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_ERROR_EXPR_SPECIFIC < %t.body.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_BODY_I_4 > %t.body.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.body.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_LOCAL < %t.body.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_BODY_I_5 > %t.body.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.body.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_LOCAL < %t.body.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=C_STYLE_FOR_BODY_I_6 > %t.body.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.body.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_LOCAL < %t.body.txt


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_EACH_EXPR_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FOR_EACH_EXPR_2 | %FileCheck %s -check-prefix=COND_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_EXPR_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_EXPR_2 | %FileCheck %s -check-prefix=COND_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_1 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_2 | %FileCheck %s -check-prefix=COND_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_3 | %FileCheck %s -check-prefix=COND_COMMON


// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_I_1 > %t.where.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.where.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_LOCAL < %t.where.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_I_2 > %t.where.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.where.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_LOCAL < %t.where.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SWITCH_CASE_WHERE_EXPR_I_J_1 > %t.where.txt
// RUN: %FileCheck %s -check-prefix=COND_COMMON < %t.where.txt
// RUN: %FileCheck %s -check-prefix=WITH_I_INT_LOCAL < %t.where.txt
// RUN: %FileCheck %s -check-prefix=WITH_J_INT < %t.where.txt

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_IF_1 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_IF_2 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_IF_3 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_IF_4 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_WHILE_1 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_WHILE_2 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_WHILE_3 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_WHILE_4 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_GUARD_1 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_GUARD_2 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_GUARD_3 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_GUARD_4 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_GUARD_5 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_GUARD_6 | %FileCheck %s -check-prefix=UNRESOLVED_B
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_GUARD_7 | %FileCheck %s -check-prefix=UNRESOLVED_B


struct FooStruct {
  var instanceVar : Int
  init(_: Int = 0) { }
  func boolGen() -> Bool { return false }
  func intGen() -> Int { return 1 }
}

func testIf1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if #^COND_IF_1^#
}

func testIf2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if #^COND_IF_2^# {
  }
}

func testIf3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if var z = #^COND_IF_3^# {
  }
}

func testIf4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if var z = #^COND_IF_4^# {
  }
}

func testIfElseIf1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if #^COND_IF_ELSE_IF_1^#
}

func testIfElseIf2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if #^COND_IF_ELSE_IF_2^# {
  }
}

func testIfElseIf3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if true {
  } else if #^COND_IF_ELSE_IF_3^#
}

func testIfElseIf4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if true {
  } else if #^COND_IF_ELSE_IF_4^# {
  }
}

func testIfElseIf5(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if var z = #^COND_IF_ELSE_IF_5^#
}

func testIfElseIf6(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if let z = #^COND_IF_ELSE_IF_6^# {
  }
}


func testWhile1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while #^COND_WHILE_1^#
}

func testWhile2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while #^COND_WHILE_2^# {
  }
}

func testWhile3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while var z = #^COND_WHILE_3^#
}

func testWhile4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while let z = #^COND_WHILE_4^#
}

func testRepeatWhile1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  repeat {
  } while #^COND_DO_WHILE_1^#
}

func testRepeatWhile2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  repeat {
  } while localFooObject.#^COND_DO_WHILE_2^#
}

func testCStyleForInit1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for #^C_STYLE_FOR_INIT_1^#
}

func testCStyleForInit2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for #^C_STYLE_FOR_INIT_2^#;
}

func testCStyleForInit3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for #^C_STYLE_FOR_INIT_3^# ;
}

func testCStyleForCond1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; #^C_STYLE_FOR_COND_1^#
}

func testCStyleForCond2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; #^C_STYLE_FOR_COND_2^#;
}

func testCStyleForCond3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; #^C_STYLE_FOR_COND_3^# ;
}

func testCStyleForCondI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; #^C_STYLE_FOR_COND_I_1^#
}

func testCStyleForCondI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = unknown_var; #^C_STYLE_FOR_COND_I_2^#
}

func testCStyleForCondIE1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0, e = 10; true; #^C_STYLE_FOR_COND_I_E_1^#
}

func testCStyleForIncr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; ; #^C_STYLE_FOR_INCR_1^#
}

func testCStyleForIncr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; ; #^C_STYLE_FOR_INCR_2^# {
  }
}

func testCStyleForIncrI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; true; #^C_STYLE_FOR_INCR_I_1^#
}

func testCStyleForIncrI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; i != 10; #^C_STYLE_FOR_INCR_I_2^#
}

func testCStyleForIncrI3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; unknown_var != 10; #^C_STYLE_FOR_INCR_I_3^#
}

func testCStyleForIncrI4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = unknown_var; unknown_var != 10; #^C_STYLE_FOR_INCR_I_4^#
}

func testCStyleForIncrIE1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0, e = 10; true; #^C_STYLE_FOR_INCR_I_E_1^#
}

func testCStyleForBodyI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0 {
    #^C_STYLE_FOR_BODY_I_1^#
  }
}

func testCStyleForBodyI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; {
    #^C_STYLE_FOR_BODY_I_2^#
  }
}

func testCStyleForBodyI3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = unknown_var; {
    #^C_STYLE_FOR_BODY_I_3^#
  }
}

func testCStyleForBodyI4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; ; {
    #^C_STYLE_FOR_BODY_I_4^#
  }
}

func testCStyleForBodyI5(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; unknown_var != 10; {
    #^C_STYLE_FOR_BODY_I_5^#
  }
}

func testCStyleForBodyI6(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; ; unknown_var++ {
    #^C_STYLE_FOR_BODY_I_6^#
  }
}

func testForEachExpr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for i in #^FOR_EACH_EXPR_1^#
}

func testForEachExpr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for i in #^FOR_EACH_EXPR_2^# {
  }
}

func testSwitchExpr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch #^SWITCH_EXPR_1^#
}

func testSwitchExpr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch #^SWITCH_EXPR_2^# {
  }
}

func testSwitchCaseWhereExpr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_1^#
  }
}

func testSwitchCaseWhereExpr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_2^#:
  }
}

func testSwitchCaseWhereExpr3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_3^# :
  }
}

func testSwitchCaseWhereExprI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (var i, 0) where #^SWITCH_CASE_WHERE_EXPR_I_1^#
  }
}

func testSwitchCaseWhereExprI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, var i) where #^SWITCH_CASE_WHERE_EXPR_I_2^#
  }
}

func testSwitchCaseWhereExprIJ1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (var i, var j) where #^SWITCH_CASE_WHERE_EXPR_I_J_1^#
  }
}

// COND_COMMON: Begin completions
// COND_COMMON-DAG: Literal[Boolean]/None: true[#Bool#]{{; name=.+$}}
// COND_COMMON-DAG: Literal[Boolean]/None: false[#Bool#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[LocalVar]/Local:        fooObject[#FooStruct#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[LocalVar]/Local:        localInt[#Int#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[LocalVar]/Local:        localFooObject[#FooStruct#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[Struct]/CurrModule:     FooStruct[#FooStruct#]{{; name=.+$}}
// COND_COMMON: End completions

// COND-WITH-RELATION: Begin completions
// COND-WITH-RELATION-DAG: Literal[Boolean]/None/TypeRelation[Identical]: true[#Bool#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Literal[Boolean]/None/TypeRelation[Identical]: false[#Bool#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[LocalVar]/Local:        fooObject[#FooStruct#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[LocalVar]/Local:        localInt[#Int#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[LocalVar]/Local:        localFooObject[#FooStruct#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[Struct]/CurrModule:     FooStruct[#FooStruct#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/NotRecommended/TypeRelation[Invalid]: testIf2({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/NotRecommended/TypeRelation[Invalid]: testWhile3({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/NotRecommended/TypeRelation[Invalid]: testIfElseIf5({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/NotRecommended/TypeRelation[Invalid]: testCStyleForIncrIE1({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}

// COND-WITH-RELATION1: Begin completions
// COND-WITH-RELATION1-DAG: Decl[InstanceVar]/CurrNominal:      instanceVar[#Int#]{{; name=.+$}}
// COND-WITH-RELATION1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Identical]: boolGen()[#Bool#]{{; name=.+$}}
// COND-WITH-RELATION1-DAG: Decl[InstanceMethod]/CurrNominal:   intGen()[#Int#]{{; name=.+$}}
// COND-WITH-RELATION1: End completions

// WITH_I_INT_LOCAL: Decl[LocalVar]/Local: i[#Int#]{{; name=.+$}}

// WITH_I_INT_EXPR_SPECIFIC: Decl[LocalVar]/ExprSpecific: i[#Int#]{{; name=.+$}}

// WITH_I_ERROR_LOCAL: Decl[LocalVar]/Local: i[#<<error type>>#]{{; name=.+$}}

// WITH_I_ERROR_EXPR_SPECIFIC: Decl[LocalVar]/ExprSpecific: i[#<<error type>>#]{{; name=.+$}}

// WITH_J_INT: Decl[LocalVar]/Local: j[#Int#]{{; name=.+$}}

// WITH_I_E_EXPR_SPECIFIC: Decl[LocalVar]/ExprSpecific: i[#Int#]{{; name=.+$}}
// WITH_I_E_EXPR_SPECIFIC: Decl[LocalVar]/Local:        e[#Int#]{{; name=.+$}}

enum A { case aaa }
enum B { case bbb }
// UNRESOLVED_B-NOT: aaa
// UNRESOLVED_B: Decl[EnumElement]/ExprSpecific:     bbb[#B#]; name=bbb
// UNRESOLVED_B-NOT: aaa

struct AA {
  func takeEnum(_: A) {}
}
struct BB {
  func takeEnum(_: B) {}
}
func testUnresolvedIF1(x: BB) {
  if x.takeEnum(.#^UNRESOLVED_IF_1^#)
}
func testUnresolvedIF2(x: BB) {
  if true, x.takeEnum(.#^UNRESOLVED_IF_2^#)
}
func testUnresolvedIF3(x: BB) {
  if true, x.takeEnum(.#^UNRESOLVED_IF_3^#) {}
}
func testUnresolvedIF4(x: BB) {
  if let x.takeEnum(.#^UNRESOLVED_IF_4^#)
}

func testUnresolvedWhile1(x: BB) {
  while x.takeEnum(.#^UNRESOLVED_WHILE_1^#)
}
func testUnresolvedWhile2(x: BB) {
  while true, x.takeEnum(.#^UNRESOLVED_WHILE_2^#)
}
func testUnresolvedWhile3(x: BB) {
  while let x.takeEnum(.#^UNRESOLVED_WHILE_3^#)
}
func testUnresolvedWhile4(x: BB) {
  while true, x.takeEnum(.#^UNRESOLVED_WHILE_4^#) {}
}

func testUnresolvedGuard1(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_1^#)
}
func testUnresolvedGuard2(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_2^#) {}
}
func testUnresolvedGuard3(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_3^#) else
}
func testUnresolvedGuard4(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_4^#) else {}
}
func testUnresolvedGuard5(x: BB) {
  guard true, x.takeEnum(.#^UNRESOLVED_GUARD_5^#)
}
func testUnresolvedGuard6(x: BB) {
  guard let x.takeEnum(.#^UNRESOLVED_GUARD_6^#)
}
func testUnresolvedGuard7(x: BB) {
  guard let x.takeEnum(.#^UNRESOLVED_GUARD_7^#) else {}
}
