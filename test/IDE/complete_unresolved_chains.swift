// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_CHAIN_1 | %FileCheck %s -check-prefix=UNRESOLVED_CHAIN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_CHAIN_2 | %FileCheck %s -check-prefix=UNRESOLVED_CHAIN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_CHAIN_3 | %FileCheck %s -check-prefix=UNRESOLVED_CHAIN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_CHAIN_4 | %FileCheck %s -check-prefix=UNRESOLVED_CHAIN_1
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_CHAIN_5 | %FileCheck %s -check-prefix=UNRESOLVED_CHAIN_2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_CHAIN_6 | %FileCheck %s -check-prefix=UNRESOLVED_CHAIN_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=UNRESOLVED_CHAIN_7 | %FileCheck %s -check-prefix=UNRESOLVED_CHAIN_3
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=DOUBLY_NESTED| %FileCheck %s -check-prefix=DOUBLY_NESTED

struct ChainStruct1 {
  static var chain2 = ChainStruct2()
  static func chain2Func() -> ChainStruct2 { ChainStruct2() }
}

struct ChainStruct2 {
  var chainStruct1 = ChainStruct1()
  var chainEnum = ChainEnum.case1
  var chainStruct2: ChainStruct2 { ChainStruct2() }
  func chainStruct1Func() -> ChainStruct1 { ChainStruct1() }
}

enum ChainEnum {
  case case1
  var chainStruct2: ChainStruct2 { ChainStruct2() }
  func chainStruct2Func() -> ChainStruct2 { ChainStruct2() }
}

func testChains() {
  let _: ChainStruct1 = .chain2.#^UNRESOLVED_CHAIN_1^#
  let _: ChainStruct1 = .chain2.chainStruct2.#^UNRESOLVED_CHAIN_2^#
  let _: ChainStruct1 = .chain2Func().#^UNRESOLVED_CHAIN_3^#
  let _: ChainStruct1 = .chain2Func().#^UNRESOLVED_CHAIN_4^#
  let _: ChainEnum = .case1.#^UNRESOLVED_CHAIN_5^#
  let _: ChainEnum = .case1.chainStruct2.#^UNRESOLVED_CHAIN_6^#
  let _: ChainEnum = .case1.chainStruct2.#^UNRESOLVED_CHAIN_7^#
}

// UNRESOLVED_CHAIN_1: Begin completions, 5 items
// UNRESOLVED_CHAIN_1-DAG: Keyword[self]/CurrNominal:          self[#ChainStruct2#]; name=self
// UNRESOLVED_CHAIN_1-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: chainStruct1[#ChainStruct1#];
// UNRESOLVED_CHAIN_1-DAG: Decl[InstanceVar]/CurrNominal:      chainEnum[#ChainEnum#];
// UNRESOLVED_CHAIN_1-DAG: Decl[InstanceVar]/CurrNominal:      chainStruct2[#ChainStruct2#];
// UNRESOLVED_CHAIN_1-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Convertible]: chainStruct1Func()[#ChainStruct1#];

// UNRESOLVED_CHAIN_2: Begin completions, 5 items
// UNRESOLVED_CHAIN_2-DAG: Keyword[self]/CurrNominal:          self[#ChainEnum#]; name=self
// UNRESOLVED_CHAIN_2-DAG: Decl[InstanceVar]/CurrNominal:      chainStruct2[#ChainStruct2#]; name=chainStruct2
// UNRESOLVED_CHAIN_2-DAG: Decl[InstanceMethod]/CurrNominal:   chainStruct2Func()[#ChainStruct2#]; name=chainStruct2Func()
// UNRESOLVED_CHAIN_2-DAG: Decl[InstanceVar]/CurrNominal:      hashValue[#Int#]; name=hashValue
// UNRESOLVED_CHAIN_2-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#into: &Hasher#})[#Void#]; name=hash(into:)

// UNRESOLVED_CHAIN_3: Begin completions, 5 items
// UNRESOLVED_CHAIN_3-DAG: Decl[InstanceVar]/CurrNominal:      chainStruct1[#ChainStruct1#]; name=chainStruct1
// UNRESOLVED_CHAIN_3-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: chainEnum[#ChainEnum#]; name=chainEnum
// UNRESOLVED_CHAIN_3-DAG: Decl[InstanceVar]/CurrNominal:      chainStruct2[#ChainStruct2#]; name=chainStruct2
// UNRESOLVED_CHAIN_3-DAG: Decl[InstanceMethod]/CurrNominal:   chainStruct1Func()[#ChainStruct1#]; name=chainStruct1Func()

class Outer {
  class Inner: Outer {
    class InnerInner: Inner {}
    static var outer = Outer()
    static var inner = Inner()
    static func makeOuter() -> Outer { Outer() }
    static func makeInner() -> Inner { Inner() }
  }
}

func testDoublyNestedType() {
    let _: Outer = .Inner.#^DOUBLY_NESTED^#
}

// DOUBLY_NESTED: Begin completions, 8 items
// DOUBLY_NESTED-DAG: Keyword[self]/CurrNominal:          self[#Outer.Inner.Type#]; name=self
// DOUBLY_NESTED-DAG: Decl[Class]/CurrNominal/TypeRelation[Convertible]: InnerInner[#Outer.Inner.InnerInner#];
// DOUBLY_NESTED-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Convertible]: outer[#Outer#];
// DOUBLY_NESTED-DAG: Decl[StaticVar]/CurrNominal/TypeRelation[Convertible]: inner[#Outer.Inner#];
// DOUBLY_NESTED-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]: makeOuter()[#Outer#];
// DOUBLY_NESTED-DAG: Decl[StaticMethod]/CurrNominal/TypeRelation[Convertible]: makeInner()[#Outer.Inner#];
// DOUBLY_NESTED-DAG: Decl[Constructor]/CurrNominal/TypeRelation[Convertible]: init()[#Outer.Inner#];
// DOUBLY_NESTED-DAG: Decl[Class]/Super/TypeRelation[Convertible]: Inner[#Outer.Inner#];

