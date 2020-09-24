// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %S/Inputs/mock-sdk -F %S/Inputs/mock-sdk -code-completion-token=TYPE_GLOBAL | %FileCheck %s --check-prefix=TYPE_GLOBAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %S/Inputs/mock-sdk -F %S/Inputs/mock-sdk -code-completion-token=EXPR_GLOBAL | %FileCheck %s --check-prefix=EXPR_GLOBAL
// RUN: %target-swift-ide-test -code-completion -source-filename %s -I %S/Inputs/mock-sdk -F %S/Inputs/mock-sdk -code-completion-token=EXPR_MEMBER | %FileCheck %s --check-prefix=EXPR_MEMBER

import OverlayTest

func testGlobalType() {
    let _: #^TYPE_GLOBAL^#
// TYPE_GLOBAL: Begin completions
// TYPE_GLOBAL-NOT: OverlayTest[#Module#]
// TYPE_GLOBAL-DAG: Decl[Module]/None:                  OverlayTest[#Module#];
// TYPE_GLOBAL-NOT: OverlayTest[#Module#]
// TYPE_GLOBAL-DAG: Decl[Struct]/OtherModule[OverlayTest.Overlayed]: Overlayed[#Overlayed#];
// TYPE_GLOBAL: End completions
}
func testGlobalExpr() {
    let _ = #^EXPR_GLOBAL^#
// EPXR_GLOBAL: Begin completions
// EXPR_GLOBAL-NOT: OverlayTest[#Module#]
// EXPR_GLOBAL-DAG: Decl[Module]/None:                  OverlayTest[#Module#];
// EXPR_GLOBAL-NOT: OverlayTest[#Module#]
// EXPR_GLOBAL-DAG: Decl[Struct]/OtherModule[OverlayTest.Overlayed]: Overlayed[#Overlayed#];
// EXPR_GLOBAL-DAG: Decl[FreeFunction]/OtherModule[OverlayTest]: createOverlayedInOverlay()[#Overlayed#];
// EXPR_GLOBAL-DAG: Decl[FreeFunction]/OtherModule[OverlayTest.Overlayed]: createOverlayed()[#Overlayed#];
// EPXR_GLOBAL: End completions
}
func testGlobalExpr(value: Overlayed) {
    value.#^EXPR_MEMBER^#
// EXPR_MEMBER: Begin completions, 6 items
// EXPR_MEMBER-DAG: Keyword[self]/CurrNominal:          self[#Overlayed#]; name=self
// EXPR_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      x[#Double#]; name=x
// EXPR_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      y[#Double#]; name=y
// EXPR_MEMBER-DAG: Decl[InstanceVar]/CurrNominal:      z[#Double#]; name=z
// EXPR_MEMBER-DAG: Decl[InstanceMethod]/CurrNominal:   inOverlayFunc()[#Void#]; name=inOverlayFunc()
// EXPR_MEMBER-DAG: Decl[InstanceMethod]/CurrNominal:   inOriginalFunc()[#Double#]; name=inOriginalFunc()
// EXPR_MEMBER: End completions
}
