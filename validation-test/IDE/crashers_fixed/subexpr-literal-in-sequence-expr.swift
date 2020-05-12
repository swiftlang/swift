// RUN: %target-swift-ide-test -code-completion -code-completion-token=A -source-filename=%s | %FileCheck %s
// RUN: %target-swift-ide-test -code-completion -code-completion-token=B -source-filename=%s | %FileCheck %s

func test1() {
  1 + [0]#^A^#
}
func test2() {
  "" + [""]#^B^#
}
// Sanity check results.
// CHECK: Decl[InstanceVar]/CurrNominal/IsSystem:      .startIndex[#Int#]; name=startIndex
