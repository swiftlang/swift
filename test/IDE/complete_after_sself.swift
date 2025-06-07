// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INIT_MyClass | %FileCheck %s --check-prefixes=CHECK-MyClass
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_DEINIT_MyClass | %FileCheck %s --check-prefixes=CHECK-MyClass
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INIT_MyClass | %FileCheck %s --check-prefixes=CHECK-MyClass
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INSTANCEMETHOD_MyClass | %FileCheck %s --check-prefixes=CHECK-MyClass
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_STATICMETHOD_MyClass | %FileCheck %s --check-prefixes=CHECK-MyClass

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INIT_MyStruct | %FileCheck %s --check-prefixes=CHECK-MyStruct
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_DEINIT_MyStruct | %FileCheck %s --check-prefixes=CHECK-MyStruct
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INIT_MyStruct | %FileCheck %s --check-prefixes=CHECK-MyStruct
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INSTANCEMETHOD_MyStruct | %FileCheck %s --check-prefixes=CHECK-MyStruct
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_STATICMETHOD_MyStruct | %FileCheck %s --check-prefixes=CHECK-MyStruct

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INIT_MyProto | %FileCheck %s --check-prefixes=CHECK-MyProto
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_DEINIT_MyProto | %FileCheck %s --check-prefixes=CHECK-MyProto
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INIT_MyProto | %FileCheck %s --check-prefixes=CHECK-MyProto
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_INSTANCEMETHOD_MyProto | %FileCheck %s --check-prefixes=CHECK-MyProto
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=SSELF_DOT_IN_STATICMETHOD_MyProto | %FileCheck %s --check-prefixes=CHECK-MyProto

class MyClass {
  init() {
    Self.#^SSELF_DOT_IN_INIT_MyClass^#
  }
  deinit {
    Self.#^SSELF_DOT_IN_DEINIT_MyClass^#
  }
  func instanceMethod() {
    Self.#^SSELF_DOT_IN_INSTANCEMETHOD_MyClass^#
  }
  static func staticMethod() {
    Self.#^SSELF_DOT_IN_STATICMETHOD_MyClass^#
  }
// CHECK-MyClass: Begin completions, 5 items
// CHECK-MyClass-DAG: Keyword[self]/CurrNominal:          self[#Self.Type#];
// CHECK-MyClass-DAG: Keyword/CurrNominal:                Type[#Self.Type#];
// CHECK-MyClass-DAG: Decl[Constructor]/CurrNominal:      init()[#MyClass#];
// CHECK-MyClass-DAG: Decl[InstanceMethod]/CurrNominal:   instanceMethod({#(self): MyClass#})[#() -> Void#];
// CHECK-MyClass-DAG: Decl[StaticMethod]/CurrNominal:     staticMethod()[#Void#];
}

struct MyStruct {
  init() {
    Self.#^SSELF_DOT_IN_INIT_MyStruct^#
  }
  deinit {
    Self.#^SSELF_DOT_IN_DEINIT_MyStruct^#
  }
  func instanceMethod() {
    Self.#^SSELF_DOT_IN_INSTANCEMETHOD_MyStruct^#
  }
  static func staticMethod() {
    Self.#^SSELF_DOT_IN_STATICMETHOD_MyStruct^#
  }
// CHECK-MyStruct: Begin completions, 5 items
// CHECK-MyStruct-DAG: Keyword[self]/CurrNominal:          self[#MyStruct.Type#];
// CHECK-MyStruct-DAG: Keyword/CurrNominal:                Type[#MyStruct.Type#];
// CHECK-MyStruct-DAG: Decl[Constructor]/CurrNominal:      init()[#MyStruct#];
// CHECK-MyStruct-DAG: Decl[InstanceMethod]/CurrNominal:   instanceMethod({#(self): MyStruct#})[#() -> Void#];
// CHECK-MyStruct-DAG: Decl[StaticMethod]/CurrNominal:     staticMethod()[#Void#];
}

protocol MyProto { }
extension MyProto {
  init() {
    Self.#^SSELF_DOT_IN_INIT_MyProto^#
  }
  deinit {
    Self.#^SSELF_DOT_IN_DEINIT_MyProto^#
  }
  func instanceMethod() {
    Self.#^SSELF_DOT_IN_INSTANCEMETHOD_MyProto^#
  }
  static func staticMethod() {
    Self.#^SSELF_DOT_IN_STATICMETHOD_MyProto^#
  }
// CHECK-MyProto: Begin completions, 5 items
// CHECK-MyProto-DAG: Keyword[self]/CurrNominal:          self[#Self.Type#];
// CHECK-MyProto-DAG: Keyword/CurrNominal:                Type[#Self.Type#];
// CHECK-MyProto-DAG: Decl[Constructor]/CurrNominal:      init()[#MyProto#];
// CHECK-MyProto-DAG: Decl[InstanceMethod]/CurrNominal:   instanceMethod({#(self): MyProto#})[#() -> Void#];
// CHECK-MyProto-DAG: Decl[StaticMethod]/CurrNominal:     staticMethod()[#Void#];
}
