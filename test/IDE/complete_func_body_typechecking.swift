// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_1 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_2 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_3 | FileCheck %s -check-prefix=ERROR_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_4 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_5 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_6 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_7 | FileCheck %s -check-prefix=ERROR_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IN_CONSTRUCTOR_1 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IN_CONSTRUCTOR_2 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IN_DESTRUCTOR_1 | FileCheck %s -check-prefix=FOO_STRUCT_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IF_1 | FileCheck %s -check-prefix=LOCALS_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IF_2 | FileCheck %s -check-prefix=LOCALS_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IF_3 | FileCheck %s -check-prefix=LOCALS_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IF_4 | FileCheck %s -check-prefix=LOCALS_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IF_IN_CONSTRUCTOR_1 | FileCheck %s -check-prefix=LOCALS_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TC_VAR_IF_IN_DESTRUCTOR_1 | FileCheck %s -check-prefix=LOCALS_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_1 | FileCheck %s -check-prefix=LOCALS_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_2 | FileCheck %s -check-prefix=LOCALS_COMMON

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_IN_CONSTRUCTOR_1 | FileCheck %s -check-prefix=LOCALS_COMMON
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=EXPR_POSTFIX_BEGIN_IN_DESTRUCTOR_1 | FileCheck %s -check-prefix=LOCALS_COMMON

struct FooStruct {
  var instanceVar = 0
  init(_ instanceVar: Int = 0) { }
  func instanceFunc0() {}

  func builderFunc1() -> FooStruct {
    return self
  }

  func builderFunc2(a: Int) -> FooStruct {
    return self
  }
}

// FOO_STRUCT_COMMON: Begin completions
// FOO_STRUCT_COMMON-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{$}}
// FOO_STRUCT_COMMON-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{$}}
// FOO_STRUCT_COMMON-NEXT: Decl[InstanceMethod]/CurrNominal: builderFunc1()[#FooStruct#]{{$}}
// FOO_STRUCT_COMMON-NEXT: Decl[InstanceMethod]/CurrNominal: builderFunc2({#(a): Int#})[#FooStruct#]{{$}}
// FOO_STRUCT_COMMON-NEXT: End completions

// ERROR_COMMON: found code completion token
// ERROR_COMMON-NOT: Begin completions

// LOCALS_COMMON: Begin completions
// LOCALS_COMMON-DAG: Decl[LocalVar]/Local: localInt[#Int#]{{$}}
// LOCALS_COMMON-DAG: Decl[LocalVar]/Local: localFooObject[#FooStruct#]{{$}}
// LOCALS_COMMON: End completions

func testTypecheckVar1() {
  var localFooObject = FooStruct()
  localFooObject.#^TC_VAR_1^#
}

func testTypecheckVar2() {
  var localFooObject = FooStruct(42)
  localFooObject.#^TC_VAR_2^#
}

func testTypecheckVar3() {
  // FIXME: We don't display any useful completions here, although we could --
  // it is obvious that 'foo' could only have type 'FooStruct'.
  //
  // In any case, ensure that we don't crash.
  var localFooObject = FooStruct(unknown_var)
  localFooObject.#^TC_VAR_3^#
}

func testTypecheckVar4() {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  localFooObject.#^TC_VAR_4^#
}

func testTypecheckVar5() {
  var localInt = 42
  FooStruct(localInt).#^TC_VAR_5^#
}

func testTypecheckVar6() {
  var localInt = 42
  FooStruct(localInt).builderFunc1().#^TC_VAR_6^#
}

func testTypecheckVar7() {
  // FIXME: We don't display any useful completions here, although we could --
  // it is obvious that the expression could only have type 'FooStruct'.
  //
  // In any case, ensure that we don't crash.
  var localInt = 42
  FooStruct(localInt).builderFunc2(unknown_var).#^TC_VAR_7^#
}

class TestTypeCheckVarInConstructor1 {
  init() {
    var localFooObject = FooStruct()
    localFooObject.#^TC_VAR_IN_CONSTRUCTOR_1^#
  }
}

class TestTypeCheckVarInConstructor2 {
  init { // Missing parameters
    var localFooObject = FooStruct()
    localFooObject.#^TC_VAR_IN_CONSTRUCTOR_2^#
  }
}

class TestTypeCheckVarInDestructor1 {
  deinit {
    var localFooObject = FooStruct()
    localFooObject.#^TC_VAR_IN_DESTRUCTOR_1^#
  }
}

func testTypecheckVarInIf1() {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
    #^TC_VAR_IF_1^#
  }
}

func testTypecheckVarInIf2() {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {
  } else {
    #^TC_VAR_IF_2^#
  }
}

func testTypecheckVarInIf3() {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if {
    #^TC_VAR_IF_3^#
  }
}

func testTypecheckVarInIf4() {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if {
  } else {
    #^TC_VAR_IF_4^#
  }
}

class TestTypeCheckVarInIfInConstructor1 {
  init() {
    var localInt = 42
    var localFooObject = FooStruct(localInt)
    if true {
      #^TC_VAR_IF_IN_CONSTRUCTOR_1^#
    }
  }
}

class TestTypeCheckVarInIfInDestructor1 {
  deinit {
    var localInt = 42
    var localFooObject = FooStruct(localInt)
    if true {
      #^TC_VAR_IF_IN_DESTRUCTOR_1^#
    }
  }
}

func testExprPostfixBegin1() {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  #^EXPR_POSTFIX_BEGIN_1^#
}

func testExprPostfixBegin2() {
  var localInt = 42
  var localFooObject = FooStruct(localInt)
  if true {}
  #^EXPR_POSTFIX_BEGIN_2^#
}

class TestTypeCheckExprPostfixBeginInConstructor1 {
  init() {
    var localInt = 42
    var localFooObject = FooStruct(localInt)
    #^EXPR_POSTFIX_BEGIN_IN_CONSTRUCTOR_1^#
  }
}

class TestTypeCheckExprPostfixBeginInDestructor1 {
  deinit {
    var localInt = 42
    var localFooObject = FooStruct(localInt)
    #^EXPR_POSTFIX_BEGIN_IN_DESTRUCTOR_1^#
  }
}

