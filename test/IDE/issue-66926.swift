// RUN: %batch-code-completion

func test(_ value: Int?) {
#if true
  switch value {
  case .some(let underlyingValue) where #^COMPLETE^#underlyingValue is Int:
    break
  }
#endif
}

// COMPLETE: Decl[LocalVar]/Local: underlyingValue[#Int#]; name=underlyingValue
