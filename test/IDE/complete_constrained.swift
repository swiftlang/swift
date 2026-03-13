// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MYSTRUCT_INT_DOT | %FileCheck %s -check-prefix=MYSTRUCT_INT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=META_MYSTRUCT_INT_DOT | %FileCheck %s -check-prefix=META_MYSTRUCT_INT_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITIONAL_OVERLOAD_ARG | %FileCheck %s -check-prefix=CONDITIONAL_OVERLOAD_ARG
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITIONAL_OVERLOAD_INIT_ARG | %FileCheck %s -check-prefix=CONDITIONAL_OVERLOAD_ARG
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITIONAL_INAPPLICABLE_ARG | %FileCheck %s -check-prefix=CONDITIONAL_INAPPLICABLE_ARG
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=CONDITIONAL_DEPENDENT_TYPEALIAS | %FileCheck %s -check-prefix=CONDITIONAL_DEPENDENT_TYPEALIAS

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

protocol Proto_Int {}
extension Proto_Int {
  func conditional_Int() -> Int { return 1 }
}
protocol Proto_String {}
extension Proto_String {
  func conditional_String() -> Int { return 1 }
}
extension MyStruct: Proto_Int where T == Int{}
extension MyStruct: Proto_String where T == String {}

func foo(s: MyStruct<Int>) {
  let _ = s.#^MYSTRUCT_INT_DOT^#
// MYSTRUCT_INT_DOT: Begin completions, 7 items
// MYSTRUCT_INT_DOT-DAG: Keyword[self]/CurrNominal:          self[#MyStruct<Int>#]; name=self
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   methodWithConstrainedGenericParam({#x: SomeProto#})[#Int#]; name=methodWithConstrainedGenericParam(x:)
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_TEqInt_None()[#Int#]; name=concreteExt_TEqInt_None()
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_None_TEqInt({#(x): U#})[#Int#]; name=concreteExt_None_TEqInt(:)
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_AssocEqInt_None()[#Int#]; name=protoExt_AssocEqInt_None()
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_None_AssocEqInt({#(x): U#})[#Int#]; name=protoExt_None_AssocEqInt(:)
// MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         conditional_Int()[#Int#]; name=conditional_Int()

  let _ = MyStruct<Int>.#^META_MYSTRUCT_INT_DOT^#
// META_MYSTRUCT_INT_DOT: Begin completions, 11 items
// META_MYSTRUCT_INT_DOT-DAG: Keyword[self]/CurrNominal:          self[#MyStruct<Int>.Type#]; name=self
// META_MYSTRUCT_INT_DOT-DAG: Keyword/CurrNominal:                Type[#MyStruct<Int>.Type#]; name=Type
// META_MYSTRUCT_INT_DOT-DAG: Decl[TypeAlias]/CurrNominal:        Assoc[#Int#]; name=Assoc
// META_MYSTRUCT_INT_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#int: U#})[#MyStruct<Int>#]; name=init(int:)
// META_MYSTRUCT_INT_DOT-DAG: Decl[Constructor]/CurrNominal:      init({#withConstrainedGenericParam: SomeProto#})[#MyStruct<Int>#]; name=init(withConstrainedGenericParam:)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   methodWithConstrainedGenericParam({#(self): MyStruct<Int>#})[#(x: SomeProto) -> Int#]; name=methodWithConstrainedGenericParam(:)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_TEqInt_None({#(self): MyStruct<Int>#})[#() -> Int#]; name=concreteExt_TEqInt_None(:)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   concreteExt_None_TEqInt({#(self): MyStruct<Int>#})[#(U) -> Int#]; name=concreteExt_None_TEqInt(:)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_AssocEqInt_None({#(self): MyStruct<Int>#})[#() -> Int#]; name=protoExt_AssocEqInt_None(:)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         protoExt_None_AssocEqInt({#(self): MyStruct<Int>#})[#(U) -> Int#]; name=protoExt_None_AssocEqInt(:)
// META_MYSTRUCT_INT_DOT-DAG: Decl[InstanceMethod]/Super:         conditional_Int({#(self): MyStruct<Int>#})[#() -> Int#]; name=conditional_Int(:)
}

// https://github.com/apple/swift/issues/52344

enum Fruit     { case apple }
enum Vegetable { case broccoli }
enum Meat      { case chicken }

protocol EatsFruit      { }
protocol EatsVegetables { }
protocol EatsMeat       { }

struct Chef <Client> { }

extension Chef where Client: EatsFruit {
  init(_ favorite: Fruit) {}
  func cook(_ food: Fruit) { }
}
extension Chef where Client: EatsVegetables {
  init(_ favorite: Vegetable) {}
  func cook(_ food: Vegetable) { }
}
extension Chef where Client: EatsMeat {
  init(favorite: Meat) {}
  func cook(_ food: Meat) { }
  func eat(_ food: Meat) {}
}

struct Vegetarian: EatsFruit, EatsVegetables { }

func testVegetarian(chef: Chef<Vegetarian>) {
  chef.cook(.#^CONDITIONAL_OVERLOAD_ARG^#)
// CONDITIONAL_OVERLOAD_ARG: Begin completions, 4 items
// CONDITIONAL_OVERLOAD_ARG-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]:    apple[#Fruit#]; name=apple
// CONDITIONAL_OVERLOAD_ARG-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]:    hash({#(self): Fruit#})[#(into: inout Hasher) -> Void#]; name=hash(:)
// CONDITIONAL_OVERLOAD_ARG-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]:    broccoli[#Vegetable#]; name=broccoli
// CONDITIONAL_OVERLOAD_ARG-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]:    hash({#(self): Vegetable#})[#(into: inout Hasher) -> Void#]; name=hash(:)

  var chefMeta: Chef<Vegetarian>.Type = Chef<Vegetarian>.self
  let _ = chefMeta.init(.#^CONDITIONAL_OVERLOAD_INIT_ARG^#)

  chef.eat(.#^CONDITIONAL_INAPPLICABLE_ARG^#)
// Note: 'eat' is from an inapplicable constrained extension. We complete as if the user intends to address that later
//       (e.g. by adding the missing 'Meat' conformance to 'Vegetarian' - clearly not the intention here - but replace 'Meat' with 'Equatable').
// CONDITIONAL_INAPPLICABLE_ARG: Begin completions, 2 items
// CONDITIONAL_INAPPLICABLE_ARG-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: chicken[#Meat#]; name=chicken
// CONDITIONAL_INAPPLICABLE_ARG-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): Meat#})[#(into: inout Hasher) -> Void#]; name=hash(:)
}

// rdar://problem/53401609
protocol MyProto {
  associatedtype Index
}
extension MyProto where Index: Strideable, Index.Stride == Int {
  func indices() {}
}
struct MyConcrete {}
extension MyConcrete: MyProto {
  typealias Index = Int
}
func testHasIndex(value: MyConcrete) {
  value.#^CONDITIONAL_DEPENDENT_TYPEALIAS^#
// CONDITIONAL_DEPENDENT_TYPEALIAS: Begin completions, 2 items
// CONDITIONAL_DEPENDENT_TYPEALIAS-DAG: Keyword[self]/CurrNominal:          self[#MyConcrete#];
// CONDITIONAL_DEPENDENT_TYPEALIAS-DAG: Decl[InstanceMethod]/Super:         indices()[#Void#];
}
