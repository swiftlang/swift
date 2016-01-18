
#if BEFORE

public struct ChangeStoredToComputed {
  public init() {
    celsius = 0
  }

  public var celsius: Int

  public var fahrenheit: Int {
    get {
      return (celsius * 9) / 5 + 32
    }
    set {
      celsius = ((newValue - 32) * 5) / 9
    }
  }
}

#else

public struct ChangeStoredToComputed {
  public init() {
    fahrenheit = 32
  }

  public var celsius: Int {
    get {
      return ((fahrenheit - 32) * 5) / 9
    }
    set {
      fahrenheit = (newValue * 9) / 5 + 32
    }
  }

  public var fahrenheit: Int = 32
}

#endif


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

public func getProperty(c: ChangeEmptyToNonEmpty) -> Int {
  return c.property
}


#if BEFORE

public struct AddStoredProperty {
  public init() {
    count = 0
  }

  public var count: Int
}

#else

public struct AddStoredProperty {
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
}

#endif
