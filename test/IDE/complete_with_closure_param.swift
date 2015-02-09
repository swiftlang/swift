// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | FileCheck %s

typealias FunctionTypealias = (Int, Int) -> Bool
typealias OptionalFunctionTypealias = ((Int, Int) -> Bool)?

struct FooStruct {
  func instanceMethod1(callback: (Int, Int) -> Void) {}
  func instanceMethod2(callback: ((Int, Int) -> Void)?) {}
  func instanceMethod3(callback: ((Int, Int) -> Void)??) {}
  func instanceMethod4(callback: ((Int, Int) -> Void)!) {}
  func instanceMethod5(callback: FunctionTypealias) {}
  func instanceMethod6(callback: FunctionTypealias?) {}
  func instanceMethod7(callback: OptionalFunctionTypealias) {}
}

FooStruct().#^COMPLETE^#

// CHECK: Begin completions, 7 items
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod1({#(callback): (Int, Int) -> Void##(Int, Int) -> Void#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod2({#(callback): ((Int, Int) -> Void)?##(Int, Int) -> Void#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod3({#(callback): ((Int, Int) -> Void)??##(Int, Int) -> Void#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod4({#(callback): ((Int, Int) -> Void)!##(Int, Int) -> Void#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod5({#(callback): FunctionTypealias##(Int, Int) -> Bool#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod6({#(callback): FunctionTypealias?##(Int, Int) -> Bool#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod7({#(callback): OptionalFunctionTypealias##(Int, Int) -> Bool#})[#Void#]{{; name=.+$}}
// CHECK: End completions
