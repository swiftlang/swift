public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public enum ResilientEnum {
  case first
  case second

#if AFTER
  @_weakLinked case third
#endif

  case fourth
  case fifth
}
