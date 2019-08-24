
public struct RemoveConformance {
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

#if BEFORE
protocol InternalProtocol {
  var x: Int { get set }
  var y: Int { get set }
  var z: Int { get set }
}

extension RemoveConformance : InternalProtocol {}
#endif
