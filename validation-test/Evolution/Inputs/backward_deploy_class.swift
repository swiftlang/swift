public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public struct ResilientStruct {
  public init() {}
}

#if AFTER
@_weakLinked public class ResilientClass {
  public init() {}

  public func fn(_ x: Int) {}

  public var storedProp: Int = 0

  public var computedProp: Int {
    get { return 0 }
    set { }
  }

  public subscript(idx: Int) -> Int {
    get { return 0 }
    set { }
  }
}

@_weakLinked @_fixed_layout public class FixedLayoutClass {
  public init() {}

  public func fn(_ x: Int) {}

  // Make the first field resilient so that the second one has to be accessed
  // with a field offset global
  private var resilientField = ResilientStruct()

  public var storedProp: Int = 0

  public var computedProp: Int {
    get { return 0 }
    set { }
  }

  public subscript(idx: Int) -> Int {
    get { return 0 }
    set { }
  }
}
#endif

// Overriding a weak-linked method
open class OpenClass {
  public init() {}

  open func oldMethod() {}

#if AFTER
  @_weakLinked open func newMethod() {}
#endif
}

// Inserting a superclass
open class Top {
  public init() {}

  open func topMethod() {}
}

#if BEFORE

open class Bottom : Top {}

#else

@_weakLinked open class Middle : Top {
  open func middleMethod() {}
}

open class Bottom : Middle {}

#endif
