func compute(_ fn: () -> Int32) -> Int32 {
  return fn() + privateFn()
}

fileprivate func privateFn() -> Int32 {
  return 40
}
