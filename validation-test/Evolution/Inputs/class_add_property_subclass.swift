
#if BEFORE

open class AddStoredProperty {
  public let x: Int

  public init(x: Int) {
    self.x = x
  }

  public func get() -> (Int, Int) {
    return (x, -x)
  }
}

#else

open class AddStoredProperty {
  public let x: Int
  public let y: Int

  public init(x: Int) {
    self.x = x
    self.y = -x
  }

  public func get() -> (Int, Int) {
    return (x, y)
  }
}

#endif
