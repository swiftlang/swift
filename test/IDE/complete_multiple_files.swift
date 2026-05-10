// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | %FileCheck %s -check-prefix=T1
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=T2 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | %FileCheck %s -check-prefix=T2
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_1 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | %FileCheck %s -check-prefix=TOP_LEVEL_1
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MODULE_SCOPED %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | %FileCheck %s -check-prefix=MODULE_SCOPED

// RUN: %empty-directory(%t)
// RUN: echo "" > %t/empty.swift
// RUN: %swift-ide-test --code-completion --code-completion-token VAR_INITIALIZED_BY_CALLING_CLOSURE --source-filename %s --second-source-filename %t/empty.swift | %FileCheck %s -check-prefix=VAR_INITIALIZED_BY_CALLING_CLOSURE

func testObjectExpr() {
  fooObject.#^T1^#
}
// T1: Begin completions, 3 items
// T1-DAG: Keyword[self]/CurrNominal: self[#FooStruct#]; name=self
// T1-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// T1-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}

func testGenericObjectExpr() {
  genericFooObject.#^T2^#
}
// T2: Begin completions, 3 items
// T2-DAG: Keyword[self]/CurrNominal: self[#GenericFooStruct<Void>#]; name=self
// T2-DAG: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// T2-DAG: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}

func topLevel1() {
  #^TOP_LEVEL_1^#
}
// TOP_LEVEL_1-NOT: ERROR
// TOP_LEVEL_1: Literal[Boolean]/None: true[#Bool#]{{; name=.+$}}
// TOP_LEVEL_1-NOT: true
// TOP_LEVEL_1-NOT: ERROR

func moduleScoped() {
  swift_ide_test.#^MODULE_SCOPED^#
}
// MODULE_SCOPED-NOT: ERROR
// MODULE_SCOPED: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// MODULE_SCOPED-NOT: ERROR

// https://github.com/apple/swift/issues/57800

enum Foo {
    case bar
}

var _: Void = {
    let foo: Foo = .#^VAR_INITIALIZED_BY_CALLING_CLOSURE^#
}()

// VAR_INITIALIZED_BY_CALLING_CLOSURE:     Begin completions, 2 items
// VAR_INITIALIZED_BY_CALLING_CLOSURE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: bar[#Foo#];
// VAR_INITIALIZED_BY_CALLING_CLOSURE-DAG: Decl[InstanceMethod]/CurrNominal/TypeRelation[Invalid]: hash({#(self): Foo#})[#(into: inout Hasher) -> Void#];
