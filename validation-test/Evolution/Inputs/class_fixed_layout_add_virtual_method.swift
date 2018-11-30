
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if BEFORE

@_fixed_layout
public class AddVirtualMethod {
  public init() {}

  public func firstMethod() -> Int {
    return 1
  }

  public func secondMethod() -> Int {
    return 2
  }
}

#else

@_fixed_layout
public class AddVirtualMethod {
  // Note: methods were re-ordered, new method added in the middle
  public func secondMethod() -> Int {
    return 2
  }

  public func thirdMethod() -> Int {
    return 3
  }

  public func firstMethod() -> Int {
    return 1
  }

  public init() {}
}

#endif
