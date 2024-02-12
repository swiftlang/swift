// RUN: %empty-directory(%t)
// RUN: %swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck --completion-output-dir %t

protocol Shape {}

struct Square : Shape {}

struct Test {
  let protocolType: Shape
  let structType: Square

  func testAny() -> any Shape {
    return self.#^WITH_ANY_CONTEXTUAL_TYPE^#
// WITH_ANY_CONTEXTUAL_TYPE-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: protocolType[#any Shape#];
// WITH_ANY_CONTEXTUAL_TYPE-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: structType[#Square#];
  }

  func testSome() -> some Shape {
    return self.#^WITH_SOME_CONTEXTUAL_TYPE^#
// WITH_SOME_CONTEXTUAL_TYPE-DAG: Decl[InstanceVar]/CurrNominal: protocolType[#any Shape#];
// WITH_SOME_CONTEXTUAL_TYPE-DAG: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: structType[#Square#];
  }
}
