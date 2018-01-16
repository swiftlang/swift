
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public class AddOverrideBase {
  public init() {}

  public var number: Int {
    return 42
  }
}

#if BEFORE

public final class AddOverrideDerived : AddOverrideBase {}

#else

public final class AddOverrideDerived : AddOverrideBase {
  override public var number: Int {
    return 69
  }
}

#endif

@_transparent public func getNumber() -> Int {
  return AddOverrideDerived().number
}
