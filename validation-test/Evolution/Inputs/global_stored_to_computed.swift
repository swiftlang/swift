#if BEFORE

public var globalStoredToComputed: Int = 0

#else

var _realValue: Int = 0

public var globalStoredToComputed: Int {
  get {
    return _realValue
  }
  set {
    _realValue = newValue
  }
}

#endif
