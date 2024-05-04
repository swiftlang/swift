// RUN: %target-swift-frontend -emit-sil %s

@_transparent func f(a: Any.Type) -> Any {
  return a
}

func g(a: Any.Type) {
  f(a: a)
}
