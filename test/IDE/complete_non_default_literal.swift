// RUN: %batch-code-completion

extension Double {
  var doubleOnlyMember: Double { self }
}

struct CustomInt: ExpressibleByIntegerLiteral {
  public init() {}
  public init(integerLiteral value: Int) {  }
  var customIntMember: CustomInt { .init() }
}

func test() {
  let double: Double = 2.#^DOUBLE_CONTEXTUAL_TYPE^#
  // DOUBLE_CONTEXTUAL_TYPE: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: doubleOnlyMember[#Double#];

  let int: Int = 2.#^INT_CONTEXTUAL_TYPE^#
  // INT_CONTEXTUAL_TYPE: Decl[InstanceVar]/CurrNominal: doubleOnlyMember[#Double#];

  let customInt: CustomInt = 2.#^CUSTOM_INT^#
  // CUSTOM_INT-NOT: customIntMember
}
