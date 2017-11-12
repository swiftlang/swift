
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

open class AddOverrideBase {
  public init() {}

  public var description: String {
    return "Base"
  }
}

#if BEFORE

open class AddOverrideGeneric<T> : AddOverrideBase {}

open class AddOverrideConcrete : AddOverrideBase {}

#else

open class AddOverrideGeneric<T> : AddOverrideBase {
  override public var description: String {
    return "Generic"
  }
}

open class AddOverrideConcrete : AddOverrideBase {
  override public var description: String {
    return "Concrete"
  }
}

#endif
