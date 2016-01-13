
public struct ChangeSize {
  public init() {
    _value = 0
  }

  public var value: Int32 {
    get { return Int32(_value) }
    set { _value = T(newValue) }
  }

#if BEFORE
  typealias T = Int32
#else
  typealias T = Int64
#endif

  private var _value: T
}

@_fixed_layout public struct ChangeFieldOffsetsOfFixedLayout {
  public init() {
    v1 = ChangeSize()
    v2 = ChangeSize()
  }

  public var v1: ChangeSize
  public var v2: ChangeSize

  public func getTotal() -> Int32 {
    return v1.value + v2.value
  }
}
