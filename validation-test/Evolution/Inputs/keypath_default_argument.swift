
#if BEFORE

public struct ResilientStruct {
  public init() {}

  public var value: Int = 0

  public mutating func change(_ keyPath: WritableKeyPath<ResilientStruct, Int> = \.value) -> Int {
    self[keyPath: keyPath] = 10
    return value
  }
}

#else

public struct ResilientStruct {
  public init() {}

  private var _value: Int = 0

  public var value: Int {
    get {
      return -_value
    }
    set {
      _value = -newValue
    }
  }

  public mutating func change(_ keyPath: WritableKeyPath<ResilientStruct, Int> = \.value) -> Int {
    self[keyPath: keyPath] = 10
    return value
  }
}

#endif
