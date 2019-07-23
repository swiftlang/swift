
#if BEFORE

@frozen
public struct ChangeStoredToComputed {
  public static var celsius: Int = 0

  public static var fahrenheit: Int {
    get {
      return (celsius * 9) / 5 + 32
    }
    set {
      celsius = ((newValue - 32) * 5) / 9
    }
  }
}

#else

@frozen
public struct ChangeStoredToComputed {
  public static var celsius: Int {
    get {
      return ((fahrenheit - 32) * 5) / 9
    }
    set {
      fahrenheit = (newValue * 9) / 5 + 32
    }
  }

  public static var fahrenheit: Int = 32
}

#endif
