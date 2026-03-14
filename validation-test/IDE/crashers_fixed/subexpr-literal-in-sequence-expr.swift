// RUN: %batch-code-completion

func test1() {
  1 + [0]#^A^#
// A: Decl[InstanceVar]/CurrNominal/IsSystem/TypeRelation[Convertible]:      .startIndex[#Int#]; name=startIndex
}
func test2() {
  "" + [""]#^B^#
// B: Decl[InstanceVar]/CurrNominal/IsSystem:      .startIndex[#Int#]; name=startIndex
}

