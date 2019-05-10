public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if AFTER
@_weakLinked @usableFromInline func weakFunction() -> String {
  return "new"
}

@_alwaysEmitIntoClient public func serializedFunction() -> String {
  if getVersion() == 1 {
    return weakFunction()
  }
  return "old"
}
#endif
