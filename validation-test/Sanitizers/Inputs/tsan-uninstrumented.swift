// This is uninstrumented code used for testing calls into uninstrumented
// modules.

public struct UninstrumentedStruct {
  public init() { }

  public func read() -> Int {
    return 0
  }

  public mutating func mutate() { }

  public var storedProperty1: Int = 7
  public var storedProperty2: Int = 22

  public subscript(index: Int) -> Int {
    get { return 0 }
    set(newValue) { }
  }

  public var storedClass: UninstrumentedClass? = nil
}

public class UninstrumentedClass {
  public init() { }

  public func read() -> Int {
    return 0
  }

  public func mutate() { }

  public var storedProperty1: Int = 7
  public var storedProperty2: Int = 22

  public subscript(index: Int) -> Int {
    get { return 0 }
    set(newValue) { }
  }

  public var storedStructProperty: UninstrumentedStruct = UninstrumentedStruct()

  public var computedStructProperty: UninstrumentedStruct {
    get { return UninstrumentedStruct() }
    set { }
  }
}

public func uninstrumentedTakesInout(_ i: inout Int) { }

public var storedGlobalInUninstrumentedModule1: Int = 7
public var storedGlobalInUninstrumentedModule2: Int = 88

public var computedGlobalInUninstrumentedModule1: Int {
  get { return 0 }
  set { }
}

public var computedGlobalInUninstrumentedModule2: Int {
  get { return 0 }
  set { }
}

public func uninstrumentedBlackHole<T>(_ p: T) { }
