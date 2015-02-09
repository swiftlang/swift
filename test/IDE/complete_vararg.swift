// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_1 | FileCheck %s -check-prefix=TOP_LEVEL_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OBJ_DOT_1 | FileCheck %s -check-prefix=OBJ_DOT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FREE_FUNC_1 | FileCheck %s -check-prefix=FREE_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=FREE_FUNC_2 | FileCheck %s -check-prefix=FREE_FUNC_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CURRIED_FREE_FUNC_1 | FileCheck %s -check-prefix=CURRIED_FREE_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CURRIED_FREE_FUNC_2 | FileCheck %s -check-prefix=CURRIED_FREE_FUNC_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INIT_1 | FileCheck %s -check-prefix=INIT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD_1 | FileCheck %s -check-prefix=METHOD_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=METHOD_2 | FileCheck %s -check-prefix=METHOD_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SUBSCRIPT_1 | FileCheck %s -check-prefix=SUBSCRIPT_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GENERIC_FREE_FUNC_1 | FileCheck %s -check-prefix=GENERIC_FREE_FUNC_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=INTERESTING_TYPE_1 | FileCheck %s -check-prefix=INTERESTING_TYPE_1

func freeFunc1(#x: Int...) { }
func freeFunc2(#x: Int, #y: Int...) { }
func freeFunc3(#w: Int...)(#x: Int...) { }

class C {
  init(x: Int...) { }
  func method1(#x: Int...) { }
  func method2(#x: Int, y: Int...) { }
  func method3(#w: Int...)(#x: Int...) { }
  subscript(i: Int...) -> Int { return 0 }
}

func genericFreeFunc1<T>(#t: T...) { }
func interestingType1(#x: (Int, (Int, String))...) { }

func testTopLevel() {
  #^TOP_LEVEL_1^#
}
// TOP_LEVEL_1: Begin completions
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/CurrModule:      freeFunc1({#x: Int...#})[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/CurrModule:      freeFunc2({#x: Int#}, {#y: Int...#})[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/CurrModule:      freeFunc3({#w: Int...#})[#(x: Int...) -> Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/CurrModule:      genericFreeFunc1({#t: T...#})[#Void#]{{; name=.+$}}
// TOP_LEVEL_1-DAG: Decl[FreeFunction]/CurrModule:      interestingType1({#x: (Int, (Int, String))...#})[#Void#]{{; name=.+$}}
// TOP_LEVEL_1: End completions

var obj = C()
func testObjDot1() {
  obj.#^OBJ_DOT_1^#
}
// OBJ_DOT_1: Begin completions
// OBJ_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   method1({#x: Int...#})[#Void#]{{; name=.+$}}
// OBJ_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   method2({#x: Int#}, {#y: Int...#})[#Void#]{{; name=.+$}}
// OBJ_DOT_1-DAG: Decl[InstanceMethod]/CurrNominal:   method3({#w: Int...#})[#(x: Int...) -> Void#]{{; name=.+$}}
// OBJ_DOT_1: End completions

func testFreeFunc() {
  freeFunc1(#^FREE_FUNC_1^#
  freeFunc2(#^FREE_FUNC_2^#
}
// FREE_FUNC_1: Begin completions, 1 items
// FREE_FUNC_1: Pattern/ExprSpecific:               ['(']{#x: Int...#})[#Void#]{{; name=.+$}}
// FREE_FUNC_1: End completions
// FREE_FUNC_2: Begin completions, 1 items
// FREE_FUNC_2: Pattern/ExprSpecific:               ['(']{#x: Int#}, {#y: Int...#})[#Void#]{{; name=.+$}}
// FREE_FUNC_2: End completions

func testCurriedFreeFunc() {
  freeFunc3(#^CURRIED_FREE_FUNC_1^#
  freeFunc3(w: 3, 4)(#^CURRIED_FREE_FUNC_2^#
}
// CURRIED_FREE_FUNC_1: Begin completions, 1 items
// CURRIED_FREE_FUNC_1: Pattern/ExprSpecific:               ['(']{#w: Int...#})[#(x: Int...) -> ()#]{{; name=.+$}}
// CURRIED_FREE_FUNC_1: End completions
// CURRIED_FREE_FUNC_2: Begin completions, 1 items
// CURRIED_FREE_FUNC_2: Pattern/ExprSpecific:               ['(']{#x: Int...#})[#Void#]{{; name=.+$}}
// CURRIED_FREE_FUNC_2: End completions

func testInit() {
  let c =C(#^INIT_1^#
}
// INIT_1: Begin completions, 1 items
// INIT_1: Decl[Constructor]/CurrNominal:      ['(']{#x: Int...#})[#C#]{{; name=.+$}}
// INIT_1: End completions

func testMethod() {
  obj.method1(#^METHOD_1^#
  obj.method2(#^METHOD_2^#
}
// METHOD_1: Begin completions, 1 items
// METHOD_1: Pattern/ExprSpecific:               ['(']{#x: Int...#})[#Void#]{{; name=.+$}}
// METHOD_1: End completions
// METHOD_2: Begin completions, 1 items
// METHOD_2: Pattern/ExprSpecific:               ['(']{#x: Int#}, {#y: Int...#})[#Void#]{{; name=.+$}}
// METHOD_2: End completions

func testSubscript() {
  obj#^SUBSCRIPT_1^#
}
// SUBSCRIPT_1: Begin completions
// SUBSCRIPT_1: Decl[Subscript]/CurrNominal:        [{#Int...#}][#Int#]{{; name=.+$}}
// SUBSCRIPT_1: End completions

func testGenericFreeFunc() {
  genericFreeFunc1(#^GENERIC_FREE_FUNC_1^#
}
// GENERIC_FREE_FUNC_1: Begin completions, 1 items
// GENERIC_FREE_FUNC_1: Pattern/ExprSpecific:               ['(']{#t: τ_0_0...#})[#Void#]{{; name=.+$}}
// GENERIC_FREE_FUNC_1: End completions


func testInterestingType() {
  interestingType1(#^INTERESTING_TYPE_1^#
}
// INTERESTING_TYPE_1: Begin completions, 1 items
// INTERESTING_TYPE_1: Pattern/ExprSpecific:               ['(']{#x: (Int, (Int, String))...#})[#Void#]{{; name=.+$}}
// INTERESTING_TYPE_1: End completions
