func compute(fn: () -> Int32) -> Int32 {
  return fn() + privateFn()
}

private func privateFn() -> Int32 {
  return 40
}
