// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -verify -parse %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_1 | FileCheck %s -check-prefix=DEFAULT_ARGS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_INIT_1 | FileCheck %s -check-prefix=DEFAULT_ARG_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_INIT_2 | FileCheck %s -check-prefix=DEFAULT_ARG_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_INIT_3 | FileCheck %s -check-prefix=DEFAULT_ARG_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_INIT_4 | FileCheck %s -check-prefix=DEFAULT_ARG_INIT

func freeFuncWithDefaultArgs1(
    a: Int, b: Int = 42, file: String = __FILE__, line: Int = __LINE__,
    column: Int = __COLUMN__, function: String = __FUNCTION__) {
}
func freeFuncWithDefaultArgs2(file: String = __FILE__) {}

// NO_ERRORS_UP_TO_HERE

func testDefaultArgs1() {
  #^DEFAULT_ARGS_1^#
}
// DEFAULT_ARGS_1: Begin completions
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs1({#(a): Int#}, {#b: Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs2()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1: End completions

let globalVar = 1
func testDefaultArgInit1(x = #^DEFAULT_ARG_INIT_1^#) { }
func testDefaultArgInit2(_: Int = #^DEFAULT_ARG_INIT_2^#) { }
func testDefaultArgInit3(Int = #^DEFAULT_ARG_INIT_3^#) { }
func testDefaultArgInit4(x: Int = #^DEFAULT_ARG_INIT_4^#) { }
// DEFAULT_ARG_INIT: Begin completions
// DEFAULT_ARG_INIT: Decl[GlobalVar]/CurrModule:         globalVar[#Int#]{{; name=.+$}}
// DEFAULT_ARG_INIT: End completions
