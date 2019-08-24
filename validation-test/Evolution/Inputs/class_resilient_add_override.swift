
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

  public var count: Int = 0
}

#if BEFORE

open class AddOverrideGeneric<T> : AddOverrideBase {}

open class AddOverrideConcrete : AddOverrideBase {}

#else

open class AddOverrideGeneric<T> : AddOverrideBase {
  override public var description: String {
    return "Generic"
  }

  override public var count: Int {
    get { return super.count * 2 }
    set { super.count = newValue / 2 }
  }
}

open class AddOverrideConcrete : AddOverrideBase {
  override public var description: String {
    return "Concrete"
  }

  override public var count: Int {
    get { return super.count * 2 }
    set { super.count = newValue / 2 }
  }
}

#endif
