
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

open class AddOverrideBase {
  public init() {}

  open var description: String {
    return "Base"
  }
}

#if BEFORE

open class AddOverrideGeneric<T> : AddOverrideBase {}

open class AddOverrideConcrete : AddOverrideBase {}

final public class AddOverrideConcreteFinal : AddOverrideBase {}

#else

open class AddOverrideGeneric<T> : AddOverrideBase {
  override open var description: String {
    return "Generic"
  }
}

open class AddOverrideConcrete : AddOverrideBase {
  override open var description: String {
    return "Concrete"
  }
}

final public class AddOverrideConcreteFinal : AddOverrideBase {
  override public var description: String {
    return "ConcreteFinal"
  }
}

#endif
