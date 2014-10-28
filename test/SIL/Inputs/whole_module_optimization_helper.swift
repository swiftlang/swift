func compute(fn: () -> Int) -> Int {
  return fn() + privateFn()
}

private func privateFn() -> Int {
  return 40
}
