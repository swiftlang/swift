// RUN: %batch-code-completion

// rdar://146055457 - Make sure we synthesize CodingKeys

struct S: Encodable {
  var a: String
  var b: String

  private var foo: CodingKeys {
    .#^COMPLETE^#
    // COMPLETE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: a[#CodingKeys#]; name=a
    // COMPLETE-DAG: Decl[EnumElement]/CurrNominal/Flair[ExprSpecific]/TypeRelation[Convertible]: b[#CodingKeys#]; name=b
  }
}
