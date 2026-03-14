// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=GLOBAL_1 > %t
// RUN: %FileCheck %s -check-prefix=GLOBAL_1 < %t
// RUN: %FileCheck %s -check-prefix=NONTRIVIAL < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=METHOD_1 > %t
// RUN: %FileCheck %s -check-prefix=METHOD_1 < %t
// RUN: %FileCheck %s -check-prefix=NONTRIVIAL < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=METHOD_2 > %t
// RUN: %FileCheck %s -check-prefix=GLOBAL_1 < %t
// RUN: %FileCheck %s -check-prefix=METHOD_1 < %t
// RUN: %FileCheck %s -check-prefix=NONTRIVIAL < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=METHOD_3 > %t
// RUN: %FileCheck %s -check-prefix=METHOD_1 < %t
// RUN: %FileCheck %s -check-prefix=NONTRIVIAL < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=STATIC_METHOD_1 > %t
// RUN: %FileCheck %s -check-prefix=STATIC_METHOD_1 < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=METHOD_4 > %t
// RUN: %FileCheck %s -check-prefix=METHOD_4 < %t
// RUN: %FileCheck %s -check-prefix=NONTRIVIAL < %t

// RUN: %target-swift-ide-test -code-completion -source-filename=%s -code-completion-token=CLASS_METHOD_1 > %t
// RUN: %FileCheck %s -check-prefix=CLASS_METHOD_1 < %t

// NONTRIVIAL-NOT: nonTrivial{{.*}} {|}

func global1(_: ()->()) {}
func global2(label: ()->()) {}
func global3(_: () throws -> ()) rethrows {}
func global4(x: Int = 0, y: Int = 2, _: ()->()) {}
func nonTrivial1(_: (Int) -> ()) {}
func nonTrivial2(_: () -> Int) {}
func nonTrivial3(x: Int, _: () -> Int) {}

func test1() {
  #^GLOBAL_1^#
}
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global1 {|}[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global2 {|}[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global3 {|}[' rethrows'][#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global4 {|}[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global1({#() -> ()##() -> ()#})[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global2({#label: () -> ()##() -> ()#})[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global3({#() throws -> ()##() throws -> ()#})[' rethrows'][#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global4({#() -> ()##() -> ()#})[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      global4({#x: Int#}, {#y: Int#}, {#() -> ()##() -> ()#})[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      nonTrivial1({#(Int) -> ()##(Int) -> ()#})[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      nonTrivial2({#() -> Int##() -> Int#})[#Void#]
// GLOBAL_1-DAG: Decl[FreeFunction]/CurrModule:      nonTrivial3({#x: Int#}, {#() -> Int##() -> Int#})[#Void#]

struct S {
  func method1(_: ()->()) {}
  static func method2(_: ()->()) {}
  static func method3(_ a: Int = 0, _: ()->()) {}
  func nonTrivial1(_: (Int)->()) {}
  func nonTrivial2(_: @autoclosure ()->()) {}
  func test2() {
    self.#^METHOD_1^#
  }
// METHOD_1: Decl[InstanceMethod]/CurrNominal:   method1 {|}[#Void#]
// METHOD_1: Decl[InstanceMethod]/CurrNominal:   method1({#() -> ()##() -> ()#})[#Void#]
// METHOD_1: Decl[InstanceMethod]/CurrNominal:   nonTrivial1({#(Int) -> ()##(Int) -> ()#})[#Void#]
// METHOD_1: Decl[InstanceMethod]/CurrNominal:   nonTrivial2({#()#})[#Void#]

  func test3() {
    #^METHOD_2^#
  }
}

func test4() {
  S().#^METHOD_3^#
}
func test5() {
  S.#^STATIC_METHOD_1^#
}
// STATIC_METHOD_1-NOT: {|}
// STATIC_METHOD_1: Decl[StaticMethod]/CurrNominal:     method2 {|}[#Void#]
// STATIC_METHOD_1: Decl[StaticMethod]/CurrNominal:     method3 {|}[#Void#]
// STATIC_METHOD_1-NOT: {|}

class C {
  func method1(_: ()->()) {}
  class func method2(_: ()->()) {}
  func nonTrivial1(_: (Int)->()) {}
  func nonTrivial2(_: @autoclosure ()->()) {}
  func test6() {
    self.#^METHOD_4^#
  }
// METHOD_4: Decl[InstanceMethod]/CurrNominal:   method1 {|}[#Void#]
// METHOD_4: Decl[InstanceMethod]/CurrNominal:   method1({#() -> ()##() -> ()#})[#Void#]
// METHOD_4: Decl[InstanceMethod]/CurrNominal:   nonTrivial1({#(Int) -> ()##(Int) -> ()#})[#Void#]

  func test7() {
    C.#^CLASS_METHOD_1^#
  }
// CLASS_METHOD_1-NOT: {|}
// CLASS_METHOD_1: Decl[StaticMethod]/CurrNominal:     method2 {|}[#Void#]
// CLASS_METHOD_1-NOT: {|}
}
