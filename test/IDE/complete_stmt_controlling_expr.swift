// RUN: %batch-code-completion

struct FooStruct {
  var instanceVar : Int
  init(_: Int = 0) { }
  func boolGen() -> Bool { return false }
  func intGen() -> Int { return 1 }
}

func testGuard1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  guard #^COND_GUARD_1?check=COND-WITH-RELATION^#
}

func testIf1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if #^COND_IF_1?check=COND-WITH-RELATION^#
}

func testIf2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if #^COND_IF_2?check=COND-WITH-RELATION^# {
  }
}

func testIf2b(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true, #^COND_IF_2B?check=COND-WITH-RELATION^# {
  }
}

func testIf3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if var z = #^COND_IF_3?check=COND_COMMON^# {
  }
}

func testIf4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if var z = #^COND_IF_4?check=COND_COMMON^# {
  }
}

func testIfElseIf1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if #^COND_IF_ELSE_IF_1?check=COND-WITH-RELATION^#
}

func testIfElseIf2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if #^COND_IF_ELSE_IF_2?check=COND-WITH-RELATION^# {
  }
}

func testIfElseIf3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if true {
  } else if #^COND_IF_ELSE_IF_3?check=COND-WITH-RELATION^#
}

func testIfElseIf4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if true {
  } else if #^COND_IF_ELSE_IF_4?check=COND-WITH-RELATION^# {
  }
}

func testIfElseIf5(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if var z = #^COND_IF_ELSE_IF_5?check=COND_COMMON^#
}

func testIfElseIf6(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else if let z = #^COND_IF_ELSE_IF_6?check=COND_COMMON^# {
  }
}


func testWhile1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while #^COND_WHILE_1?check=COND-WITH-RELATION^#
}

func testWhile2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while #^COND_WHILE_2?check=COND-WITH-RELATION^# {
  }
}

func testWhile2b(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while true, #^COND_WHILE_2B?check=COND-WITH-RELATION^# {
  }
}

func testWhile3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while var z = #^COND_WHILE_3?check=COND_COMMON^#
}

func testWhile4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  while let z = #^COND_WHILE_4?check=COND_COMMON^#
}

func testRepeatWhile1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  repeat {
  } while #^COND_DO_WHILE_1?check=COND-WITH-RELATION^#
}

func testRepeatWhile2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  repeat {
  } while localFooObject.#^COND_DO_WHILE_2?check=COND-WITH-RELATION1^#
}

func testForeachPattern1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for #^FOREACH_PATTERN_1^#
// FOREACH_PATTERN_1: Begin completions, 4 items
// FOREACH_PATTERN_1-DAG: Keyword[try]/None:                  try; name=try
// FOREACH_PATTERN_1-DAG: Keyword/None:                       await; name=await
// FOREACH_PATTERN_1-DAG: Keyword[var]/None:                  var; name=var
// FOREACH_PATTERN_1-DAG: Keyword[case]/None:                 case; name=case
}

func testForeachPattern2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for try #^FOREACH_PATTERN_2^#
// FOREACH_PATTERN_2: Begin completions, 3 items
// FOREACH_PATTERN_2-DAG: Keyword/None:                       await; name=await
// FOREACH_PATTERN_2-DAG: Keyword[var]/None:                  var; name=var
// FOREACH_PATTERN_2-DAG: Keyword[case]/None:                 case; name=case
}

func testForeachPattern3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for try await #^FOREACH_PATTERN_3^#
// FOREACH_PATTERN_3: Begin completions, 2 items
// FOREACH_PATTERN_3-DAG: Keyword[var]/None:                  var; name=var
// FOREACH_PATTERN_3-DAG: Keyword[case]/None:                 case; name=case
}

func testForeachPattern4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var #^FOREACH_PATTERN_4?check=COND_NONE^#
}

func testCStyleForCond1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; #^C_STYLE_FOR_COND_1?check=COND_COMMON^#
}

func testCStyleForCond2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; #^C_STYLE_FOR_COND_2?check=COND_COMMON^#;
}

func testCStyleForCond3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; #^C_STYLE_FOR_COND_3?check=COND_COMMON^# ;
}

func testCStyleForCondI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; #^C_STYLE_FOR_COND_I_1?check=COND_COMMON^#
}

func testCStyleForCondI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = unknown_var; #^C_STYLE_FOR_COND_I_2?check=COND_COMMON^#
}

func testCStyleForCondIE1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0, e = 10; true; #^C_STYLE_FOR_COND_I_E_1?check=COND_COMMON^#
}

func testCStyleForIncr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; ; #^C_STYLE_FOR_INCR_1?check=COND_COMMON^#
}

func testCStyleForIncr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for ; ; #^C_STYLE_FOR_INCR_2?check=COND_COMMON^# {
  }
}

func testCStyleForIncrI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; true; #^C_STYLE_FOR_INCR_I_1?check=COND_COMMON^#
}

func testCStyleForIncrI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; i != 10; #^C_STYLE_FOR_INCR_I_2?check=COND_COMMON^#
}

func testCStyleForIncrI3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; unknown_var != 10; #^C_STYLE_FOR_INCR_I_3?check=COND_COMMON^#
}

func testCStyleForIncrI4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = unknown_var; unknown_var != 10; #^C_STYLE_FOR_INCR_I_4?check=COND_COMMON^#
}

func testCStyleForIncrIE1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0, e = 10; true; #^C_STYLE_FOR_INCR_I_E_1?check=COND_COMMON^#
}

func testCStyleForBodyI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0 {
    #^C_STYLE_FOR_BODY_I_1?check=COND_COMMON;check=WITH_I_ERROR_LOCAL^#
  }
}

func testCStyleForBodyI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; {
    #^C_STYLE_FOR_BODY_I_2?check=COND_COMMON;check=WITH_I_ERROR_LOCAL^#
  }
}

func testCStyleForBodyI3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = unknown_var; {
    #^C_STYLE_FOR_BODY_I_3?check=COND_COMMON;check=WITH_I_ERROR_LOCAL^#
  }
}

func testCStyleForBodyI4(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; ; {
    #^C_STYLE_FOR_BODY_I_4?check=COND_COMMON;check=WITH_I_ERROR_LOCAL^#
  }
}

func testCStyleForBodyI5(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; unknown_var != 10; {
    #^C_STYLE_FOR_BODY_I_5?check=COND_COMMON;check=WITH_I_ERROR_LOCAL^#
  }
}

func testCStyleForBodyI6(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for var i = 0; ; unknown_var++ {
    #^C_STYLE_FOR_BODY_I_6?check=COND_COMMON;check=WITH_I_ERROR_LOCAL^#
  }
}

func testForEachExpr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for i in #^FOR_EACH_EXPR_1?check=COND_COMMON^#
}

func testForEachExpr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  for i in #^FOR_EACH_EXPR_2?check=COND_COMMON^# {
  }
}

func testSwitchExpr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch #^SWITCH_EXPR_1?check=COND_COMMON^#
}

func testSwitchExpr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch #^SWITCH_EXPR_2?check=COND_COMMON^# {
  }
}

func testSwitchCaseWhereExpr1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_1?check=COND_WHERE_COMMON^#
  }
}

func testSwitchCaseWhereExpr2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_2?check=COND_WHERE_COMMON^#:
  }
}

func testSwitchCaseWhereExpr3(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, 0) where #^SWITCH_CASE_WHERE_EXPR_3?check=COND_WHERE_COMMON^# :
  }
}

func testSwitchCaseWhereExprI1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (var i, 0) where #^SWITCH_CASE_WHERE_EXPR_I_1?check=COND_WHERE_COMMON;check=WITH_I_INT_LOCAL^#
  }
}

func testSwitchCaseWhereExprI2(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (0, var i) where #^SWITCH_CASE_WHERE_EXPR_I_2?check=COND_WHERE_COMMON;check=WITH_I_INT_LOCAL^#
  }
}

func testSwitchCaseWhereExprIJ1(_ fooObject: FooStruct) {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  switch (0, 42) {
    case (var i, var j) where #^SWITCH_CASE_WHERE_EXPR_I_J_1?check=COND_WHERE_COMMON;check=WITH_I_INT_LOCAL;check=WITH_J_INT^#
  }
}

// COND_NONE-NOT: Begin completions
// COND_NONE-NOT: End completions

// COND_COMMON-DAG: Literal[Boolean]/None: true[#Bool#]{{; name=.+$}}
// COND_COMMON-DAG: Literal[Boolean]/None: false[#Bool#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[LocalVar]/Local:        fooObject[#FooStruct#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[LocalVar]/Local:        localInt[#Int#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[LocalVar]/Local:        localFooObject[#FooStruct#]{{; name=.+$}}
// COND_COMMON-DAG: Decl[Struct]/CurrModule:     FooStruct[#FooStruct#]{{; name=.+$}}

// COND_WHERE_COMMON-DAG: Literal[Boolean]/None/TypeRelation[Convertible]: true[#Bool#]{{; name=.+$}}
// COND_WHERE_COMMON-DAG: Literal[Boolean]/None/TypeRelation[Convertible]: false[#Bool#]{{; name=.+$}}
// COND_WHERE_COMMON-DAG: Decl[LocalVar]/Local:        fooObject[#FooStruct#]{{; name=.+$}}
// COND_WHERE_COMMON-DAG: Decl[LocalVar]/Local:        localInt[#Int#]{{; name=.+$}}
// COND_WHERE_COMMON-DAG: Decl[LocalVar]/Local:        localFooObject[#FooStruct#]{{; name=.+$}}
// COND_WHERE_COMMON-DAG: Decl[Struct]/CurrModule:     FooStruct[#FooStruct#]{{; name=.+$}}


// COND-WITH-RELATION-DAG: Literal[Boolean]/None/TypeRelation[Convertible]: true[#Bool#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Literal[Boolean]/None/TypeRelation[Convertible]: false[#Bool#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[LocalVar]/Local:        fooObject[#FooStruct#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[LocalVar]/Local:        localInt[#Int#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[LocalVar]/Local:        localFooObject[#FooStruct#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[Struct]/CurrModule:     FooStruct[#FooStruct#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: testIf2({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: testWhile3({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: testIfElseIf5({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}
// COND-WITH-RELATION-DAG: Decl[FreeFunction]/CurrModule/TypeRelation[Invalid]: testCStyleForIncrIE1({#(fooObject): FooStruct#})[#Void#]{{; name=.+$}}

// COND-WITH-RELATION1-DAG: Decl[InstanceVar]/CurrNominal:      instanceVar[#Int#]{{; name=.+$}}
// COND-WITH-RELATION1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: boolGen()[#Bool#]{{; name=.+$}}
// COND-WITH-RELATION1-DAG: Decl[InstanceMethod]/CurrNominal:   intGen()[#Int#]{{; name=.+$}}

// WITH_I_INT_LOCAL: Decl[LocalVar]/Local: i[#Int#]{{; name=.+$}}

// WITH_I_ERROR_LOCAL: Decl[LocalVar]/Local: i[#<<error type>>#]{{; name=.+$}}

// WITH_J_INT: Decl[LocalVar]/Local: j[#Int#]{{; name=.+$}}

enum A { case aaa }
enum B { case bbb }
// UNRESOLVED_B-NOT: aaa
// UNRESOLVED_B: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]:     bbb[#B#]; name=bbb
// UNRESOLVED_B-NOT: aaa

struct AA {
  func takeEnum(_: A) {}
}
struct BB {
  func takeEnum(_: B) {}
}
func testUnresolvedIF1(x: BB) {
  if x.takeEnum(.#^UNRESOLVED_IF_1?check=UNRESOLVED_B^#)
}
func testUnresolvedIF2(x: BB) {
  if true, x.takeEnum(.#^UNRESOLVED_IF_2?check=UNRESOLVED_B^#)
}
func testUnresolvedIF3(x: BB) {
  if true, x.takeEnum(.#^UNRESOLVED_IF_3?check=UNRESOLVED_B^#) {}
}
func testUnresolvedIF4(x: BB) {
  if let x.takeEnum(.#^UNRESOLVED_IF_4?check=UNRESOLVED_B^#)
}

func testUnresolvedWhile1(x: BB) {
  while x.takeEnum(.#^UNRESOLVED_WHILE_1?check=UNRESOLVED_B^#)
}
func testUnresolvedWhile2(x: BB) {
  while true, x.takeEnum(.#^UNRESOLVED_WHILE_2?check=UNRESOLVED_B^#)
}
func testUnresolvedWhile3(x: BB) {
  while let x.takeEnum(.#^UNRESOLVED_WHILE_3?check=UNRESOLVED_B^#)
}
func testUnresolvedWhile4(x: BB) {
  while true, x.takeEnum(.#^UNRESOLVED_WHILE_4?check=UNRESOLVED_B^#) {}
}

func testUnresolvedGuard1(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_1?check=UNRESOLVED_B^#)
}
func testUnresolvedGuard2(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_2?check=UNRESOLVED_B^#) {}
}
func testUnresolvedGuard3(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_3?check=UNRESOLVED_B^#) else
}
func testUnresolvedGuard4(x: BB) {
  guard x.takeEnum(.#^UNRESOLVED_GUARD_4?check=UNRESOLVED_B^#) else {}
}
func testUnresolvedGuard5(x: BB) {
  guard true, x.takeEnum(.#^UNRESOLVED_GUARD_5?check=UNRESOLVED_B^#)
}
func testUnresolvedGuard6(x: BB) {
  guard let x.takeEnum(.#^UNRESOLVED_GUARD_6?check=UNRESOLVED_B^#)
}
func testUnresolvedGuard7(x: BB) {
  guard let x.takeEnum(.#^UNRESOLVED_GUARD_7?check=UNRESOLVED_B^#) else {}
}

func testIfLetBinding1(x: FooStruct?) {
  if let y = x, y.#^IF_LET_BIND_1?check=FOOSTRUCT_DOT_BOOL^# {}
}
func testIfLetBinding2(x: FooStruct?) {
  if let y = x, y.#^IF_LET_BIND_2?check=FOOSTRUCT_DOT_BOOL^#
}
func testIfLetBinding3(x: FooStruct?) {
  if let y = x, let z = y.#^IF_LET_BIND_3?check=FOOSTRUCT_DOT^# {}
}
func testIfLetBinding3(x: FooStruct?) {
  if let y = x, let z = y#^IF_LET_BIND_4?check=FOOSTRUCT_NODOT^# {}
}
func testGuardLetBinding1(x: FooStruct?) {
  guard let y = x, y.#^GUARD_LET_BIND_1?check=FOOSTRUCT_DOT_BOOL^# else {}
}
func testGuardLetBinding2(x: FooStruct?) {
  guard let y = x, y.#^GUARD_LET_BIND_2?check=FOOSTRUCT_DOT_BOOL^#
}
func testGuardLetBinding3(x: FooStruct?) {
  guard let y = x, y.#^GUARD_LET_BIND_3?check=FOOSTRUCT_DOT_BOOL^# else
}
func testGuardLetBinding4(x: FooStruct?) {
  guard let y = x, y.#^GUARD_LET_BIND_4?check=FOOSTRUCT_DOT_BOOL^# {}
}
func testGuardLetBinding5(x: FooStruct?) {
  guard let y = x, let z = y.#^GUARD_LET_BIND_5?check=FOOSTRUCT_DOT^# else {}
}
func testGuardLetBinding5(x: FooStruct?) {
  guard let y = x, z = y#^GUARD_LET_BIND_6?check=FOOSTRUCT_NODOT^# else {}
}
func testGuardLetBinding7(x: FooStruct?) {
  guard let boundVal = x, let other = #^GUARD_LET_BIND_7?check=FOOSTRUCT_LOCALVAL^# else {}
}
func testGuardLetBinding8(_ x: FooStruct?) {
  guard let boundVal = x, let other = testGuardLetBinding8(#^GUARD_LET_BIND_8?check=FOOSTRUCT_LOCALVAL^#) else {}
}
func testGuardCase(x:FooStruct?) {
  guard case .#^GUARD_CASE_PATTERN_1?check=OPTIONAL_FOOSTRUCT^# = x {}
}
func testGuardCase(x:FooStruct?) {
  guard case .#^GUARD_CASE_PATTERN_2?check=OPTIONAL_FOOSTRUCT^#some() = x {}
}

// FOOSTRUCT_DOT-DAG: Decl[InstanceVar]/CurrNominal:      instanceVar[#Int#];
// FOOSTRUCT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   boolGen()[#Bool#];
// FOOSTRUCT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   intGen()[#Int#];

// FOOSTRUCT_DOT_BOOL-DAG: Decl[InstanceVar]/CurrNominal:      instanceVar[#Int#];
// FOOSTRUCT_DOT_BOOL-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: boolGen()[#Bool#];
// FOOSTRUCT_DOT_BOOL-DAG: Decl[InstanceMethod]/CurrNominal:   intGen()[#Int#];

// FOOSTRUCT_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .instanceVar[#Int#];
// FOOSTRUCT_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .boolGen()[#Bool#];
// FOOSTRUCT_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .intGen()[#Int#];

// FOOSTRUCT_LOCALVAL-DAG: Decl[LocalVar]/Local{{(/TypeRelation\[Convertible\])?}}: boundVal[#FooStruct#];

// OPTIONAL_FOOSTRUCT: Begin completions, 2 items
// OPTIONAL_FOOSTRUCT-DAG: Decl[EnumElement]/CurrNominal/IsSystem/TypeRelation[Convertible]: none[#Optional<FooStruct>#]; name=none
// OPTIONAL_FOOSTRUCT-DAG: Decl[EnumElement]/CurrNominal/IsSystem/TypeRelation[Convertible]: some({#FooStruct#})[#Optional<FooStruct>#]; name=some()

func returnOpt() -> String? { nil }
func returnOptTuple() -> (String, String)? { nil}
func test_rdar86050684() {
  guard let local1 = returnOpt() else { return }
  guard let (local2, _) =  returnOptTuple() else { return }
  if let (local3, _) = returnOptTuple() {
    if case (let local4, _)? = returnOptTuple() {
      let (local5, _) = returnOptTuple() ?? ("", "")
      _ = #^RDAR86050684^#
// RDAR86050684-DAG: Decl[LocalVar]/Local:               local1[#String#];
// RDAR86050684-DAG: Decl[LocalVar]/Local:               local2[#String#];
// RDAR86050684-DAG: Decl[LocalVar]/Local:               local3[#String#];
// RDAR86050684-DAG: Decl[LocalVar]/Local:               local4[#String#];
// RDAR86050684-DAG: Decl[LocalVar]/Local:               local5[#String#];
    }
  }
}
