
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

public struct AddConformance {
  public init() {
    x = 0
    y = 0
  }

  public var x: Int
  public var y: Int

  public var z: Int {
    get { return x + y }
    set {
      x = newValue / 2
      y = newValue - x
    }
  }
}

public protocol PointLike {
  var x: Int { get set }
  var y: Int { get set }
}

public protocol Point3DLike {
  var z: Int { get set }
}

#if AFTER
extension AddConformance : PointLike {}
extension AddConformance : Point3DLike {}
#endif

public func workWithPointLike<T>(_ t: T) -> Int {
  if let p = t as? PointLike {
    return p.x * p.y
  } else {
    return 0
  }
}
