
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

@_transparent public func getBuildVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public func getFunction(_ x: Int) -> (Int) -> Int {
  // Force a re-abstraction thunk for (T -> T) => (Int -> Int) to be
  // emitted from a non-transparent context first

#if BEFORE
  func id(_ y: Int) -> Int { return x * y }
#else
  func id<T>(_ t: T) -> T { return t }
#endif

  return id
}

@_transparent public func getTransparentFunction(_ x: Int) -> (Int) -> Int {
  // The mangled name and calling convention of the local function
  // will change -- so we must serialize it and inline it into
  // the calling module

#if BEFORE
  func id(_ y: Int) -> Int { return x * y }
#else
  func id<T>(_ t: T) -> T { return t }
#endif

  return id
}
