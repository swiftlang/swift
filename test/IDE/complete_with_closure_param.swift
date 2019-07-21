// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=COMPLETE | %FileCheck %s

typealias FunctionTypealias = (Int, Int) -> Bool
typealias OptionalFunctionTypealias = ((Int, Int) -> Bool)?

struct FooStruct {
  func instanceMethod1(_ callback: (Int, Int) -> Void) {}
  func instanceMethod2(_ callback: ((Int, Int) -> Void)?) {}
  func instanceMethod3(_ callback: ((Int, Int) -> Void)??) {}
  func instanceMethod4(_ callback: ((Int, Int) -> Void)!) {}
  func instanceMethod5(_ callback: FunctionTypealias) {}
  func instanceMethod6(_ callback: FunctionTypealias?) {}
  func instanceMethod7(_ callback: OptionalFunctionTypealias) {}
}

FooStruct().#^COMPLETE^#

// CHECK: Begin completions, 8 items
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod1({#(callback): (Int, Int) -> Void##(Int, Int) -> Void#})[#Void#]; name=instanceMethod1(callback: (Int, Int) -> Void){{$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod2({#(callback): ((Int, Int) -> Void)?##(Int, Int) -> Void#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod3({#(callback): ((Int, Int) -> Void)??##(Int, Int) -> Void#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod4({#(callback): ((Int, Int) -> Void)!##(Int, Int) -> Void#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod5({#(callback): (Int, Int) -> Bool##(Int, Int) -> Bool#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod6({#(callback): FunctionTypealias?##(Int, Int) -> Bool#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Decl[InstanceMethod]/CurrNominal: instanceMethod7({#(callback): OptionalFunctionTypealias##(Int, Int) -> Bool#})[#Void#]{{; name=.+$}}
// CHECK-DAG: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// CHECK: End completions
