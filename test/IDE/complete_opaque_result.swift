// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_FUNC | %FileCheck %s -check-prefix=BEGINNING_WITH_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBAL_VAR | %FileCheck %s -check-prefix=BEGINNING_WITHOUT_SOME

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_ASSOCIATEDTYPE | %FileCheck %s -check-prefix=BEGINNING_WITHOUT_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_METHOD_REQUIREMENT | %FileCheck %s -check-prefix=BEGINNING_WITHOUT_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_VAR_REQUIREMENT | %FileCheck %s -check-prefix=BEGINNING_WITHOUT_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_SUBSCRIPT_REQUIREMENT | %FileCheck %s -check-prefix=BEGINNING_WITHOUT_SOME

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_METHOD_EXTENSION | %FileCheck %s -check-prefix=BEGINNING_WITH_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_VAR_EXTENSION | %FileCheck %s -check-prefix=BEGINNING_WITH_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=PROTOCOL_SUBSCRIPT_EXTENSION | %FileCheck %s -check-prefix=BEGINNING_WITH_SOME

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_METHOD | %FileCheck %s -check-prefix=BEGINNING_WITH_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_VAR | %FileCheck %s -check-prefix=BEGINNING_WITH_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_SUBSCRIPT | %FileCheck %s -check-prefix=BEGINNING_WITH_SOME
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=STRUCT_TYPEALIAS_RHS | %FileCheck %s -check-prefix=BEGINNING_WITHOUT_SOME

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_TestClass | %FileCheck %s -check-prefix=OVERRIDE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_TestStruct | %FileCheck %s -check-prefix=OVERRIDE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_HasTypealias | %FileCheck %s -check-prefix=OVERRIDE_HasTypealias

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_TestProtocol_DOT | %FileCheck %s -check-prefix=POSTFIX_TestProtocol_DOT
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_TestProtocol_NODOT | %FileCheck %s -check-prefix=POSTFIX_TestProtocol_NODOT

// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=OVERRIDE_TestProtocol2 | %FileCheck %s -check-prefix=OVERRIDE_TestProtocol2
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=POSTFIX_ConcreteTestProtocol2 | %FileCheck %s -check-prefix=POSTFIX_ConcreteTestProtocol2

protocol MyProtocol {
  associatedtype Mistery
}
struct MyStruct {}
enum MyEnum {}
class MyClass {}
struct ConcreteMyProtocol : MyProtocol {
  typealias Mistery = MyStruct
}

// MARK: 'some' keyword.

// BEGINNING_WITH_SOME: Begin completions
// BEGINNING_WITH_SOME-DAG: Keyword/None:                       some[#some#]; name=some
// BEGINNING_WITH_SOME-DAG: Keyword/None:                       Any[#Any#]; name=Any
// BEGINNING_WITH_SOME-DAG: Decl[Enum]/CurrModule:              MyEnum[#MyEnum#]; name=MyEnum
// BEGINNING_WITH_SOME-DAG: Decl[Class]/CurrModule:             MyClass[#MyClass#]; name=MyClass
// BEGINNING_WITH_SOME-DAG: Decl[Protocol]/CurrModule:          MyProtocol[#MyProtocol#]; name=MyProtocol
// BEGINNING_WITH_SOME-DAG: Decl[Struct]/CurrModule:            MyStruct[#MyStruct#]; name=MyStruct
// BEGINNING_WITH_SOME: End completions

// BEGINNING_WITHOUT_SOME: Begin completions
// BEGINNING_WITHOUT_SOME-NOT: Keyword/None: some
// BEGINNING_WITHOUT_SOME-DAG: Keyword/None:                       Any[#Any#]; name=Any
// BEGINNING_WITHOUT_SOME-DAG: Decl[Enum]/CurrModule:              MyEnum[#MyEnum#]; name=MyEnum
// BEGINNING_WITHOUT_SOME-DAG: Decl[Class]/CurrModule:             MyClass[#MyClass#]; name=MyClass
// BEGINNING_WITHOUT_SOME-DAG: Decl[Protocol]/CurrModule:          MyProtocol[#MyProtocol#]; name=MyProtocol
// BEGINNING_WITHOUT_SOME-DAG: Decl[Struct]/CurrModule:            MyStruct[#MyStruct#]; name=MyStruct
// BEGINNING_WITHOUT_SOME-NOT: Keyword/None: some
// BEGINNING_WITHOUT_SOME: End completions

func gloabalFunc() -> #^GLOBAL_FUNC^#
var globalVar: #^GLOBAL_VAR^#

protocol SomeProto {
  associatedtype protoAssocTy: #^PROTOCOL_ASSOCIATEDTYPE^#
  func protoMethodReq() -> #^PROTOCOL_METHOD_REQUIREMENT^#
  var protoVarReq: #^PROTOCOL_VAR_REQUIREMENT^#
  subscript(req: Int) -> #^PROTOCOL_SUBSCRIPT_REQUIREMENT^#
}

extension SomeProto {
  func protoMethodExt() -> #^PROTOCOL_METHOD_EXTENSION^#
  var protoVarExt: #^PROTOCOL_VAR_EXTENSION^#
  subscript(ext: Int) -> #^PROTOCOL_SUBSCRIPT_EXTENSION^#
}

struct SomeStruct {
  typealias TyAlias = #^STRUCT_TYPEALIAS_RHS^#
  func structMethodExt() -> #^STRUCT_METHOD^#
  var structVarExt: #^STRUCT_VAR^#
  subscript(struct: Int) -> #^STRUCT_SUBSCRIPT^#
}

// MARK: Conformance.

protocol HasAssocPlain {
  associatedtype AssocPlain
  func returnAssocPlain() -> AssocPlain
}
protocol HasAssocWithConformanceConstraint {
  associatedtype AssocWithConformanceConstraint: MyProtocol
  func returnAssocWithConformanceConstraint(fn: (Int) -> Int) -> AssocWithConformanceConstraint
}
protocol HasAssocWithSuperClassConstraint {
  associatedtype AssocWithSuperClassConstraint: MyClass
  var valAssocWithSuperClassConstraint: AssocWithSuperClassConstraint { get }
}
protocol HasAssocWithCompositionConstraint {
  associatedtype AssocWithCompositionConstraint: MyClass & MyProtocol
  subscript(idx: Int) -> AssocWithCompositionConstraint { get }
}
protocol HasAssocWithDefault {
  associatedtype AssocWithDefault = MyEnum
  func returnAssocWithDefault() -> AssocWithDefault
}
protocol HasAssocWithConstraintAndDefault {
  associatedtype AssocWithConstraintAndDefault: MyProtocol = ConcreteMyProtocol
  func returnAssocWithConstraintAndDefault() -> AssocWithConstraintAndDefault
}
protocol HasAssocWithAnyObjectConstraint {
  associatedtype AssocWithAnyObjectConstraint: AnyObject & MyProtocol 
  func returnAssocWithAnyObjectConstraint() -> AssocWithAnyObjectConstraint
}
protocol HasAssocWithConstraintOnProto where Self.AssocWithConstraintOnProto : MyProtocol {
  associatedtype AssocWithConstraintOnProto
  func returnAssocWithConstraintOnProto() -> AssocWithConstraintOnProto
}
protocol HasAssocWithSameTypeConstraint where Self.AssocWithSameTypeConstraint == ConcreteMyProtocol {
  associatedtype AssocWithSameTypeConstraint : MyProtocol
  func returnAssocWithSameTypeConstraint() -> AssocWithSameTypeConstraint
}
protocol HasAssocWithConformanceConstraintGeneric {
  associatedtype AssocWithConformanceConstraintGeneric: MyProtocol
  func returnAssocWithConformanceConstraintGeneric<T>(arg: T) -> AssocWithConformanceConstraintGeneric
}

class TestClass :
    HasAssocPlain,
    HasAssocWithConformanceConstraint,
    HasAssocWithSuperClassConstraint,
    HasAssocWithCompositionConstraint,
    HasAssocWithDefault,
    HasAssocWithConstraintAndDefault,
    HasAssocWithAnyObjectConstraint,
    HasAssocWithConstraintOnProto,
    HasAssocWithSameTypeConstraint,
    HasAssocWithConformanceConstraintGeneric {
  #^OVERRIDE_TestClass^#
// OVERRIDE: Begin completions
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocPlain() -> AssocPlain {|};
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithConformanceConstraint(fn: (Int) -> Int) -> some MyProtocol {|};
// OVERRIDE-DAG: Decl[InstanceVar]/Super:            var valAssocWithSuperClassConstraint: some MyClass;
// OVERRIDE-DAG: Decl[Subscript]/Super:              subscript(idx: Int) -> some MyClass & MyProtocol {|};
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithDefault() -> MyEnum {|};
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithConstraintAndDefault() -> ConcreteMyProtocol {|};
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithAnyObjectConstraint() -> some MyProtocol & AnyObject {|}
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithConstraintOnProto() -> some MyProtocol {|}
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithSameTypeConstraint() -> ConcreteMyProtocol {|}
// OVERRIDE-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithConformanceConstraintGeneric<T>(arg: T) -> AssocWithConformanceConstraintGeneric {|}
// OVERRIDE: End completions
}

struct TestStruct :
    HasAssocPlain,
    HasAssocWithConformanceConstraint,
    HasAssocWithSuperClassConstraint,
    HasAssocWithCompositionConstraint,
    HasAssocWithDefault,
    HasAssocWithConstraintAndDefault,
    HasAssocWithAnyObjectConstraint,
    HasAssocWithConstraintOnProto,
    HasAssocWithSameTypeConstraint,
    HasAssocWithConformanceConstraintGeneric {
  #^OVERRIDE_TestStruct^#
}

class HasTypealias : HasAssocWithConformanceConstraint {
  typealias AssocWithConformanceConstraint = ConcreteMyProtocol
  #^OVERRIDE_HasTypealias^#
// OVERRIDE_HasTypealias: Begin completions
// OVERRIDE_HasTypealias-DAG: Decl[InstanceMethod]/Super:         func returnAssocWithConformanceConstraint(fn: (Int) -> Int) -> ConcreteMyProtocol {|};
// OVERRIDE_HasTypealias: End completions
}

// MARK: Postfix expession for opaque result types.

protocol TestProtocol {
  associatedtype Assoc1
  associatedtype Assoc2: Comparable

  func foo(arg1: Assoc1, arg2: (Assoc2) -> Assoc1) -> Assoc2
  subscript(idx: Assoc1, idx2: Assoc2) -> Self { get }
  var value: (Assoc1, Assoc2) { get }
}

func vendTestProtocol() -> some TestProtocol {
  <#code#>
}

func postfixExpr() {
  var value = vendTestProtocol()
  let _ = value.#^POSTFIX_TestProtocol_DOT^#
  let _ = value#^POSTFIX_TestProtocol_NODOT^#
}
// POSTFIX_TestProtocol_DOT: Begin completions, 3 items
// POSTFIX_TestProtocol_DOT-DAG: Decl[InstanceMethod]/CurrNominal:   foo({#arg1: TestProtocol.Assoc1#}, {#arg2: (Comparable) -> TestProtocol.Assoc1##(Comparable) -> TestProtocol.Assoc1#})[#Comparable#]; name={{.*$}}
// POSTFIX_TestProtocol_DOT-DAG: Decl[InstanceVar]/CurrNominal:      value[#(TestProtocol.Assoc1, Comparable)#]; name={{.*$}}
// POSTFIX_TestProtocol_DOT-DAG: Keyword[self]/CurrNominal:          self[#TestProtocol#]; name={{.*$}}
// POSTFIX_TestProtocol_DOT: End completions

// POSTFIX_TestProtocol_NODOT: Begin completions, 5 items
// POSTFIX_TestProtocol_NODOT-DAG: Decl[InstanceMethod]/CurrNominal:   .foo({#arg1: TestProtocol.Assoc1#}, {#arg2: (Comparable) -> TestProtocol.Assoc1##(Comparable) -> TestProtocol.Assoc1#})[#Comparable#]; name={{.*$}}
// POSTFIX_TestProtocol_NODOT-DAG: Decl[Subscript]/CurrNominal:        [{#(idx): TestProtocol.Assoc1#}, {#(idx2): Comparable#}][#TestProtocol#]; name={{.*$}}
// POSTFIX_TestProtocol_NODOT-DAG: Decl[InstanceVar]/CurrNominal:      .value[#(TestProtocol.Assoc1, Comparable)#]; name={{.*$}}
// POSTFIX_TestProtocol_NODOT-DAG: BuiltinOperator/None:                = {#TestProtocol#}[#Void#]; name={{.*$}}
// POSTFIX_TestProtocol_NODOT-DAG: Keyword[self]/CurrNominal:          .self[#TestProtocol#]; name={{.*$}}
// POSTFIX_TestProtocol_NODOT-DAG: End completions

protocol TestProtocol2 {
  associatedtype Assoc: Comparable
  func foo() -> Assoc
  func bar() -> Assoc
  func baz(x: @autoclosure () -> Assoc) -> (Assoc) -> Assoc
}
extension TestProtocol2 {
  func inExt() -> Assoc { fatalError() }
}
struct ConcreteTestProtocol2: TestProtocol2 {
  func foo() -> some Comparable { 1 }
  #^OVERRIDE_TestProtocol2^#
// OVERRIDE_TestProtocol2: Begin completions
// OVERRIDE_TestProtocol2-NOT: foo()
// OVERRIDE_TestProtocol2-NOT: inExt()
// OVERRIDE_TestProtocol2-DAG: Decl[InstanceMethod]/Super:         func bar() -> Assoc {|};
// OVERRIDE_TestProtocol2-DAG: Decl[InstanceMethod]/Super:         func baz(x: @autoclosure () -> Assoc) -> (Assoc) -> Assoc {|};
// OVERRIDE_TestProtocol2-DAG: Decl[AssociatedType]/Super:         typealias Assoc = {#(Type)#};
// OVERRIDE_TestProtocol2-NOT: foo()
// OVERRIDE_TestProtocol2-NOT: inExt()
// OVERRIDE_TestProtocol2: End completions
}
func testUseTestProtocol2(value: ConcreteTestProtocol2) {
  value.#^POSTFIX_ConcreteTestProtocol2^#
// POSTFIX_ConcreteTestProtocol2: Begin completions
// POSTFIX_ConcreteTestProtocol2-DAG: Keyword[self]/CurrNominal:          self[#ConcreteTestProtocol2#];
// POSTFIX_ConcreteTestProtocol2-DAG: Decl[InstanceMethod]/CurrNominal:   foo()[#Comparable#];
// POSTFIX_ConcreteTestProtocol2-DAG: Decl[InstanceMethod]/Super:         bar()[#ConcreteTestProtocol2.Assoc#];
// POSTFIX_ConcreteTestProtocol2-DAG: Decl[InstanceMethod]/Super:         baz({#x: ConcreteTestProtocol2.Assoc#})[#(ConcreteTestProtocol2.Assoc) -> ConcreteTestProtocol2.Assoc#];
// POSTFIX_ConcreteTestProtocol2-DAG: Decl[InstanceMethod]/Super:         inExt()[#ConcreteTestProtocol2.Assoc#];
// POSTFIX_ConcreteTestProtocol2: End completions
}
