class BoxInt {
  let value: Int

  init(value: Int) {
    self.value = value
  }
}

public struct ChangeSize {
  public init(value: Int) {
#if BEFORE
    _value = BoxInt(value: value)
#else
    _value1 = Int64(value)
    _value2 = Int64(value)
#endif
  }

  public var value: Int {
#if BEFORE
    return _value.value
#else
    precondition(_value1 == _value2, "state corrupted")
    return Int(_value1)
#endif
  }

#if BEFORE
  private let _value: BoxInt
#else
  private let _value1: Int64
  private let _value2: Int64
#endif
}

@_frozen public enum SingletonEnum {
  case X(ChangeSize)
}

public func getSingletonEnumValues(_ c: ChangeSize)
    -> [SingletonEnum?] {
  return [.X(c), nil]
}

@_frozen public enum SinglePayloadEnum {
  case X(ChangeSize)
  case Y
  case Z
}

public func getSinglePayloadEnumValues(_ c: ChangeSize)
    -> [SinglePayloadEnum?] {
  return [.X(c), .Y, .Z, nil]
}

@_frozen public enum MultiPayloadEnum {
  case X(ChangeSize)
  case Y(ChangeSize)
  case Z
}

public func getMultiPayloadEnumValues(_ c: ChangeSize, _ d: ChangeSize)
    -> [MultiPayloadEnum?] {
  return [.X(c), .Y(d), .Z, nil]
}
