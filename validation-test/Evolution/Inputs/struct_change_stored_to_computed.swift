
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

  public var fahrenheit: Int
}

#endif
