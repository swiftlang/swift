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

protocol MyProtocol {}
struct MyStruct {}
enum MyEnum {}
class MyClass {}

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
  associatedtype Assoc: MyProtocol

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
