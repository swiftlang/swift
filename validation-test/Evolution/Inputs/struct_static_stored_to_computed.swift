
var _value: Int = 0

public struct ChangeStoredToComputed {

  public init() {}

#if BEFORE

  public static var value: Int = 0

#else

  public static var value: Int {
    get {
      return _value * 2
    }
    set {
      _value = newValue / 2
    }
  }

#endif

}
