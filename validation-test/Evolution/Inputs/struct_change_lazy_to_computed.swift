
#if BEFORE

public struct ChangeLazyToComputed {
  private var celsius: Int

  public init(celsius: Int) {
    self.celsius = celsius
  }

  public lazy var fahrenheit: Int = (celsius * 9) / 5 + 32
}

#else

public struct ChangeLazyToComputed {
  private var celsius: Int

  public init(celsius: Int) {
    self.celsius = celsius
  }

  public var fahrenheit: Int {
    mutating get {
      return (celsius * 9) / 5 + 32
    }
    set {
      celsius = ((newValue - 32) * 5) / 9
    }
  }
}

#endif
