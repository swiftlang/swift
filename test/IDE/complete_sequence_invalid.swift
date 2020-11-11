// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t

// GLOBAL: Decl[GlobalVar]/CurrModule:         invalidDecl[#<<error type>>#];
let invalidDecl = INVALID

struct S {
  // MEMBER: Decl[InstanceMethod]/CurrNominal:   invalidMethod()[#<<error type>>#];
  func invalidMethod() -> INVALID
}

func test() {
  #^GLOBAL_1?check=GLOBAL^#
  #^GLOBAL_2?check=GLOBAL^#
}

func testMember(val: S) {
  val.#^MEMBER_1?check=MEMBER^##^MEMBER_2?check=MEMBER^#
}
