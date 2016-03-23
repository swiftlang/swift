
@_transparent public func getBuildVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

@_transparent public func getFunction(x: Int) -> Int -> Int {
  // The mangled name and calling convention of the local function
  // will change -- so we must serialize it and inline it into
  // the calling module

#if BEFORE
  func id(y: Int) -> Int { return x * y }
#else
  func id(y: Int) -> Int { return y }
#endif
  return id
}
