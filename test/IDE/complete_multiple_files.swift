// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | FileCheck %s -check-prefix=T1
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_1 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | FileCheck %s -check-prefix=TOP_LEVEL_1
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=MODULE_SCOPED %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | FileCheck %s -check-prefix=MODULE_SCOPED

func testObjectExpr() {
  fooObject.#^T1^#
}
// T1: Begin completions
// T1-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{; name=.+$}}
// T1-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{; name=.+$}}
// T1-NEXT: End completions

func topLevel1() {
  #^TOP_LEVEL_1^#
}
// TOP_LEVEL_1: Begin completions
// TOP_LEVEL_1-NOT: ERROR
// TOP_LEVEL_1: Keyword/None: true[#Bool#]{{; name=.+$}}
// TOP_LEVEL_1-NOT: true
// TOP_LEVEL_1-NOT: ERROR
// TOP_LEVEL_1: End completions

func moduleScoped() {
  swift_ide_test.#^MODULE_SCOPED^#
}
// MODULE_SCOPED: Begin completions
// MODULE_SCOPED-NOT: ERROR
// MODULE_SCOPED: Decl[Struct]/CurrModule: FooStruct[#FooStruct#]{{; name=.+$}}
// MODULE_SCOPED-NOT: ERROR
// MODULE_SCOPED: End completions
