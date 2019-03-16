// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MYSTRUCT_INT_DOT | %FileCheck %s -check-prefix=MYSTRUCT_INT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=META_MYSTRUCT_INT_DOT | %FileCheck %s -check-prefix=META_MYSTRUCT_INT_DOT

protocol SomeProto {
  associatedtype Assoc
}

extension SomeProto where Assoc == String {
  func protoExt_AssocEqString_None() -> Int { return 1 }
}
extension SomeProto where Assoc == Int {
  func protoExt_AssocEqInt_None() -> Int { return 1 }
}
extension SomeProto where Assoc: SomeProto {
  func protoExt_AssocConformsToSomeProto_None() -> Int { return 1 }
}

extension SomeProto {
  func protoExt_None_AssocEqString<U>(_ x: U) -> Int where Assoc == String { return 1 }
  func protoExt_None_AssocEqInt<U>(_ x: U) -> Int where Assoc == Int { return 1 }
  func protoExt_None_AssocConformsToSomeProto<U>(_ x: U) -> Int where Assoc: SomeProto { return 1 }
}

struct MyStruct<T> : SomeProto {
  typealias Assoc = T
  init<U>(int: U) where T == Int {}
  init<U>(str: U) where T == String {}
  init<U: SomeProto>(withConstrainedGenericParam: U) {}
  func methodWithConstrainedGenericParam<U: SomeProto>(x: U) -> Int { return 1 }
}

extension MyStruct where T == String {
  func concreteExt_TEqString_None() -> Int { return 1 }
}
extension MyStruct where T == Int {
  func concreteExt_TEqInt_None() -> Int { return 1 }
}
extension MyStruct where T: SomeProto {
  func concreteExt_TConformsToSomeProto_None() -> Int { return 1 }
}

extension MyStruct {
  func concreteExt_None_TEqString<U>(_ x: U) -> Int where T == String { return 1 }
  func concreteExt_None_TEqInt<U>(_ x: U) -> Int where T == Int { return 1 }
  func concreteExt_None_TConformsToSomeProto<U>(_ x: U) -> Int where T: SomeProto { return 1 }
}

func foo(s: MyStruct<Int>) {
  let _ = s.#^MYSTRUCT_INT_DOT^#
// MYSTRUCT_INT_DOT: Begin completions, 6 items
// MYSTRUCT_INT_DOT-DAG: Keyword[self]/CurrNominal:          self[#MyStruct<Int>#]; name=self
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   methodWithConstrainedGenericParam({#x: SomeProto#})[#Int#]; name=methodWithConstrainedGenericParam(x: SomeProto)
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_TEqInt_None()[#Int#]; name=concreteExt_TEqInt_None()
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_None_TEqInt({#(x): U#})[#Int#]; name=concreteExt_None_TEqInt(x: U)
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_AssocEqInt_None()[#Int#]; name=protoExt_AssocEqInt_None()
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_None_AssocEqInt({#(x): U#})[#Int#]; name=protoExt_None_AssocEqInt(x: U)
// MYSTRUCT_INT_DOT: End completions

  let _ = MyStruct<Int>.#^META_MYSTRUCT_INT_DOT^#
// META_MYSTRUCT_INT_DOT: Begin completions, 10 items
// META_MYSTRUCT_INT_DOT-DAG: Keyword[self]/CurrNominal:          self[#MyStruct<Int>.Type#]; name=self
// META_MYSTRUCT_INT_DOT-DAG: Keyword/CurrNominal:                Type[#MyStruct<Int>.Type#]; name=Type
// META_MYSTRUCT_INT_DOT-DAG: Decl[TypeAlias]/CurrNominal:        Assoc[#T#]; name=Assoc
// META_MYSTRUCT_INT_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#int: U#})[#MyStruct<Int>#]; name=init(int: U)
// META_MYSTRUCT_INT_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#withConstrainedGenericParam: SomeProto#})[#MyStruct<Int>#]; name=init(withConstrainedGenericParam: SomeProto)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   methodWithConstrainedGenericParam({#(self): MyStruct<Int>#})[#(x: SomeProto) -> Int#]; name=methodWithConstrainedGenericParam(self: MyStruct<Int>)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_TEqInt_None({#(self): MyStruct<Int>#})[#() -> Int#]; name=concreteExt_TEqInt_None(self: MyStruct<Int>)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_None_TEqInt({#(self): MyStruct<Int>#})[#(U) -> Int#]; name=concreteExt_None_TEqInt(self: MyStruct<Int>)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_AssocEqInt_None({#(self): MyStruct<Int>#})[#() -> Int#]; name=protoExt_AssocEqInt_None(self: MyStruct<Int>)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_None_AssocEqInt({#(self): MyStruct<Int>#})[#(U) -> Int#]; name=protoExt_None_AssocEqInt(self: MyStruct<Int>)
// META_MYSTRUCT_INT_DOT: End completions
}
