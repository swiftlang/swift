// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -verify -typecheck %t_no_errors.swift

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_1 | %FileCheck %s -check-prefix=DEFAULT_ARGS_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_2 | %FileCheck %s -check-prefix=DEFAULT_ARGS_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-complete-add-call-with-no-default-args=false -code-completion-token=DEFAULT_ARGS_2 | %FileCheck %s --check-prefix=ONLY_DEFAULTS
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_3 | %FileCheck %s -check-prefix=DEFAULT_ARGS_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_4 | %FileCheck %s -check-prefix=DEFAULT_ARGS_4
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_5 | %FileCheck %s -check-prefix=DEFAULT_ARGS_5
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_6 | %FileCheck %s -check-prefix=DEFAULT_ARGS_6
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_7 | %FileCheck %s -check-prefix=DEFAULT_ARGS_7
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_8 > %t
// RUN: %FileCheck %s -check-prefix=DEFAULT_ARGS_8 < %t
// RUN: %FileCheck %s -check-prefix=NEGATIVE_DEFAULT_ARGS_8 < %t
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_9 > %t
// RUN: %FileCheck %s -check-prefix=DEFAULT_ARGS_9 < %t
// RUN: %FileCheck %s -check-prefix=NEGATIVE_DEFAULT_ARGS_9 < %t

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARGS_10 | %FileCheck %s -check-prefix=DEFAULT_ARGS_10

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_INIT_1 | %FileCheck %s -check-prefix=DEFAULT_ARG_INIT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_INIT_2 | %FileCheck %s -check-prefix=DEFAULT_ARG_INIT_INTCONTEXT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DEFAULT_ARG_INIT_3 | %FileCheck %s -check-prefix=DEFAULT_ARG_INIT_INTCONTEXT

func freeFuncWithDefaultArgs1(
    _ a: Int, b: Int = 42, file: String = #file, line: Int = #line,
    column: Int = #column, function: String = #function) {
}
func freeFuncWithDefaultArgs2(file: String = #file) {}
func freeFuncWithDefaultArgs3(a: Int = 0) {}
func freeFuncWithDefaultArgs4(_ a: Int, b: Int = 0, c: Int = 0) {}
func freeFuncWithDefaultArgs5(a: Int? = nil) {}
func freeFuncWithDefaultArgs6(a: [Int] = []) {}
func freeFuncWithDefaultArgs7(a: [Int:Int] = [:]) {}

struct A {
  func methodWithDefaultArgs1(a: Int = 0) {}
  static func staticMethodWithDefaultArgs1(a: Int = 0) {}
}

struct B {
  init(a: Int = 0) {}
}

class C1 {
  func methodWithDefaultArgs1(a: Int = 0) {}
  func methodWithDefaultArgsInSub1(_ a: Int) {}
}
class C2 : C1 {
  override func methodWithDefaultArgs1(a: Int) {}
  override func methodWithDefaultArgsInSub1(_ a: Int = 0) {}
}

// NO_ERRORS_UP_TO_HERE

func testDefaultArgs1() {
  #^DEFAULT_ARGS_1^#
}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs1({#(a): Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs1({#(a): Int#}, {#b: Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs2()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs3()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs3({#a: Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs4({#(a): Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs4({#(a): Int#}, {#b: Int#}, {#c: Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs5()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs5({#a: Int?#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs6()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs6({#a: [Int]#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs7()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_1-DAG: Decl[FreeFunction]/CurrModule:      freeFuncWithDefaultArgs7({#a: [Int : Int]#})[#Void#]{{; name=.+$}}

func testDefaultArgs2() {
  freeFuncWithDefaultArgs1#^DEFAULT_ARGS_2^#
}
// DEFAULT_ARGS_2-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]:      ({#(a): Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_2-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]:      ({#(a): Int#}, {#b: Int#})[#Void#]{{; name=.+$}}

// ONLY_DEFAULTS-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]:      ({#(a): Int#}, {#b: Int#})[#Void#]{{; name=.+$}}

func testDefaultArgs3() {
  freeFuncWithDefaultArgs3#^DEFAULT_ARGS_3^#
}
// DEFAULT_ARGS_3-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]:      ()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_3-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]:      ({#a: Int#})[#Void#]{{; name=.+$}}

func testDefaultArgs4(_ x: A) {
  x.#^DEFAULT_ARGS_4^#
}
// DEFAULT_ARGS_4-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgs1()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_4-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgs1({#a: Int#})[#Void#]{{; name=.+$}}

func testDefaultArgs5(_ x: A) {
  x.methodWithDefaultArgs1#^DEFAULT_ARGS_5^#
}
// DEFAULT_ARGS_5-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:      ()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_5-DAG: Decl[InstanceMethod]/CurrNominal/Flair[ArgLabels]:      ({#a: Int#})[#Void#]{{; name=.+$}}

func testDefaultArgs6() {
  A.#^DEFAULT_ARGS_6^#
}
// DEFAULT_ARGS_6-DAG: Decl[StaticMethod]/CurrNominal:      staticMethodWithDefaultArgs1()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_6-DAG: Decl[StaticMethod]/CurrNominal:      staticMethodWithDefaultArgs1({#a: Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_6-DAG: Decl[InstanceMethod]/CurrNominal:   methodWithDefaultArgs1({#(self): A#})[#(a: Int) -> Void#]{{; name=.+$}}

func testDefaultArgs7() {
  B#^DEFAULT_ARGS_7^#
}
// DEFAULT_ARGS_7-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ()[#B#]{{; name=.+$}}
// DEFAULT_ARGS_7-DAG: Decl[Constructor]/CurrNominal/Flair[ArgLabels]:      ({#a: Int#})[#B#]{{; name=.+$}}

func testDefaultArgs8(_ x: C1) {
  x.#^DEFAULT_ARGS_8^#
}
// DEFAULT_ARGS_8-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgs1()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_8-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgs1({#a: Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_8-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgsInSub1({#(a): Int#})[#Void#]{{; name=.+$}}
// NEGATIVE_DEFAULT_ARGS_8-NOT: methodWithDefaultArgsInSub1()

func testDefaultArgs9(_ x: C2) {
  x.#^DEFAULT_ARGS_9^#
}
// DEFAULT_ARGS_9-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgs1({#a: Int#})[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_9-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgsInSub1()[#Void#]{{; name=.+$}}
// DEFAULT_ARGS_9-DAG: Decl[InstanceMethod]/CurrNominal:      methodWithDefaultArgsInSub1({#(a): Int#})[#Void#]{{; name=.+$}}
// NEGATIVE_DEFAULT_ARGS_9-NOT: methodWithDefaultArgs1()

func testDefaultArgs10() {
  freeFuncWithDefaultArgs5(#^DEFAULT_ARGS_10^#)
// DEFAULT_ARGS_10-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ['('][')'][#Void#]; name=
// DEFAULT_ARGS_10-DAG: Decl[FreeFunction]/CurrModule/Flair[ArgLabels]: ['(']{#a: Int?#}[')'][#Void#]; name=a:

}

let globalVar = 1
func testDefaultArgInit1(x = #^DEFAULT_ARG_INIT_1^#) { }
func testDefaultArgInit2(_: Int = #^DEFAULT_ARG_INIT_2^#) { }
func testDefaultArgInit3(_ x: Int = #^DEFAULT_ARG_INIT_3^#) { }
// DEFAULT_ARG_INIT: Decl[GlobalVar]/CurrModule:         globalVar[#Int#]{{; name=.+$}}

// DEFAULT_ARG_INIT_INTCONTEXT: Decl[GlobalVar]/CurrModule/TypeRelation[Convertible]: globalVar[#Int#]{{; name=.+$}}
