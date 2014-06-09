// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %swift -verify -parse %t_no_errors.swift

// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_1 | FileCheck %s -check-prefix=DEFAULT_ARGS_1

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
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs1({#(a): Int#}, {#b: Int#})[#Void#]{{$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs2()[#Void#]{{$}}
// DEFAULT_ARGS_1: End completions

