
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

@_fixed_layout public struct AddRemoveConformance {
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
extension AddRemoveConformance : PointLike {}
extension AddRemoveConformance : Point3DLike {}
#endif
