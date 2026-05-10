// RUN: %batch-code-completion

// GLOBAL: Decl[GlobalVar]/CurrModule:         invalidDecl[#_#];
let invalidDecl = INVALID

struct S {
  // MEMBER: Decl[InstanceMethod]/CurrNominal:   invalidMethod()[#_#];
  func invalidMethod() -> INVALID
}

func test() {
  #^GLOBAL_1?check=GLOBAL^#
  #^GLOBAL_2?check=GLOBAL^#
}

func testMember(val: S) {
  val.#^MEMBER_1?check=MEMBER^##^MEMBER_2?check=MEMBER^#
}
