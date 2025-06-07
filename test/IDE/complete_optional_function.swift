// RUN: %batch-code-completion

func foo(_ x: ((_ x: Int, _ y: Int) -> Void)?) {
  x?(1, #^OPTIONAL_PARAMETER^#)
  // OPTIONAL_PARAMETER-DAG: Literal[Integer]/None/TypeRelation[Convertible]: 0[#Int#]; name=0
}
