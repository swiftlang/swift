
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if BEFORE

@_fixed_layout
public struct AddInitializer {
  public var x: Int

  // This could be @inlinable, but we want to force inlining to take place
  // at -Onone to get better test coverage.
  @_transparent
  public init() {
    self.x = 0
  }
}

#else

@_fixed_layout
public struct AddInitializer {
  public var x: Int = 0

  @_transparent
  public init() {}
}

#endif
