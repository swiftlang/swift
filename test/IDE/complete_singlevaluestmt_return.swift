// RUN: %batch-code-completion

struct S {
  var str: String
}

_ = {
  let k = if .random() {
    return ""
  } else {
    S()
  }
  // Make sure we can still infer 'k' here.
  return k.#^COMPLETE_ON_SVE_WITH_RET^#
  // COMPLETE_ON_SVE_WITH_RET: Decl[InstanceVar]/CurrNominal/TypeRelation[Convertible]: str[#String#]; name=str
}

