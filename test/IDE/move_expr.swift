// RUN: %batch-code-completion

func test(myParam: Int) {
  consume #^CONSUME^#
  // CONSUME: Decl[LocalVar]/Local:               myParam[#Int#]; name=myParam
}
// UNSUPPORTED: OS=windows-msvc
