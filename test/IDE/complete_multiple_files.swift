// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=T1 \
// RUN:     %S/Inputs/multiple-files-1.swift %S/Inputs/multiple-files-2.swift | FileCheck %s -check-prefix=T1

func testObjectExpr() {
  fooObject.#^T1^#
}
// T1: Begin completions
// T1-NEXT: SwiftDecl: instanceVar[#Int#]
// T1-NEXT: SwiftDecl: instanceFunc0()[#Void#]
// T1-NEXT: Keyword: metatype[#[byref(implicit)] FooStruct.metatype#]
// T1-NEXT: End completions
