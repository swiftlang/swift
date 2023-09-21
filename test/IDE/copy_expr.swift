// RUN: %batch-code-completion

func test(myParam: Int) {
  copy #^COPY^#
  // COPY: Decl[LocalVar]/Local:               myParam[#Int#]; name=myParam
}
