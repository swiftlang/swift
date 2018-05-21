#if BEFORE

var global: Int = 0

public struct ChangeEmptyToNonEmpty {
  public init() {}

  public var property: Int {
    get { return global }
    set { global = newValue }
  }
}

#else

public struct ChangeEmptyToNonEmpty {
  public init() {
    property = 0 
  }

  public var property: Int
}

#endif


#if BEFORE

public struct ChangeSize {
  public init() {
    count = 0
  }

  public var count: Int

  public func validate() -> Bool {
    return true
  }
}

#else

public struct ChangeSize {
  public init() {
    _count = 0
    _sign = 0
  }

  public var count: Int {
    get { return _count * _sign }
    set {
      if newValue < 0 {
        _count = -newValue
        _sign = -1
      } else {
        _count = newValue
        _sign = 1
      }
    }
  }

  private var _count: Int
  private var _sign: Int

  public func validate() -> Bool {
    return (padding1 == 17 &&
            padding2 == -12 &&
            padding3 == 108 &&
            padding4 == -7592)  
  }

  // Some padding to grow the struct beyond what a fixed-size buffer
  // can hold for a global. Use it as a canary to catch any memory
  // corruption issues.
  public var padding1: Int = 17
  public var padding2: Int = -12
  public var padding3: Int = 108
  public var padding4: Int = -7592
}

#endif

// Test internal global variable hidden behind a public transparent
// interface

@usableFromInline var versionedGlobal: ChangeSize = ChangeSize()

@_transparent public func getVersionedGlobal() -> ChangeSize {
  return versionedGlobal
}

@_transparent public func setVersionedGlobal(_ c: Int) {
  versionedGlobal.count = c
}
