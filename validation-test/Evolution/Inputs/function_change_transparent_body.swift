
@_transparent public func getBuildVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

