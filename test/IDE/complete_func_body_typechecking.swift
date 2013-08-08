// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_1 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_2 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_3 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_4 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_5 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_6 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_7 | FileCheck %s -check-prefix=ERROR_COMMON

// FIXME: Disabled because code completing after an expr confuses the parser
// and it suggests to continue the expression from the previous line.
//
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_1 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_1

// FIXME: Disabled because name lookup is wrong.
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_2 | FileCheck %s -check-prefix=EXPR_POSTFIX_BEGIN_2

struct FooStruct {
  var instanceVar : Int

  func instanceFunc0() {}

  func builderFunc1() -> FooStruct {
    return this
  }

  func builderFunc2(a: Int) -> FooStruct {
    return this
  }
}

// FOO_STRUCT_COMMON: Begin completions
// FOO_STRUCT_COMMON-NEXT: SwiftDecl: instanceVar[#Int#]{{$}}
// FOO_STRUCT_COMMON-NEXT: SwiftDecl: instanceFunc0()[#Void#]{{$}}
// FOO_STRUCT_COMMON-NEXT: SwiftDecl: builderFunc1()[#FooStruct#]{{$}}
// FOO_STRUCT_COMMON-NEXT: SwiftDecl: builderFunc2({#a: Int#})[#FooStruct#]{{$}}
// FOO_STRUCT_COMMON-NEXT: Keyword: metatype[#FooStruct.metatype#]{{$}}
// FOO_STRUCT_COMMON-NEXT: End completions

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

func testTypecheckVar1() {
  var foo = FooStruct()
  foo.#^TC_VAR_1^#
}

func testTypecheckVar2() {
  var foo = FooStruct(42)
  foo.#^TC_VAR_2^#
}

func testTypecheckVar3() {
  // We don't display any useful completions here, although we could -- it is
  // obvious that 'foo' could only have type 'FooStruct'.
  //
  // In any case, ensure that we don't crash.
  var foo = FooStruct(unknown_var)
  foo.#^TC_VAR_3^#
}

func testTypecheckVar4() {
  var z = 42
  var foo = FooStruct(z)
  foo.#^TC_VAR_4^#
}

func testTypecheckVar5() {
  var z = 42
  FooStruct(z).#^TC_VAR_5^#
}

func testTypecheckVar6() {
  var z = 42
  FooStruct(z).builderFunc1().#^TC_VAR_6^#
}

func testTypecheckVar7() {
  // We don't display any useful completions here, although we could -- it is
  // obvious that the expression could only have type 'FooStruct'.
  //
  // In any case, ensure that we don't crash.
  var z = 42
  FooStruct(z).builderFunc2(unknown_var).#^TC_VAR_7^#
}

func testExprPostfixBegin1() {
  var z = 42
  var foo = FooStruct(z)
  #^EXPR_POSTFIX_BEGIN_1^#
// EXPR_POSTFIX_BEGIN_1: SwiftDecl: z
// EXPR_POSTFIX_BEGIN_1: SwiftDecl: foo
}

func testExprPostfixBegin2() {
  var z = 42
  var foo = FooStruct(z)
  if true {}
  #^EXPR_POSTFIX_BEGIN_2^#
// EXPR_POSTFIX_BEGIN_2: SwiftDecl: z
// EXPR_POSTFIX_BEGIN_2: SwiftDecl: foo
}

