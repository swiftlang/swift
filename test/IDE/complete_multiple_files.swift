// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | FileCheck %s -check-prefix=T1
//
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=TOP_LEVEL_1 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | FileCheck %s -check-prefix=TOP_LEVEL_1

func testObjectExpr() {
  fooObject.#^T1^#
}
// T1: Begin completions
// T1-NEXT: Decl[InstanceVar]/CurrNominal:    instanceVar[#Int#]{{$}}
// T1-NEXT: Decl[InstanceMethod]/CurrNominal: instanceFunc0()[#Void#]{{$}}
// T1-NEXT: End completions

func topLevel1() {
  #^TOP_LEVEL_1^#
}
// TOP_LEVEL_1: Begin completions
// TOP_LEVEL_1: Decl[GlobalVar]/OtherModule: true[#Bool#]{{$}}
// TOP_LEVEL_1-NOT: true
// TOP_LEVEL_1: End completions

