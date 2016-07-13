@_exported import ObjectiveC
@_exported import CoreGraphics

public func == (lhs: CGPoint, rhs: CGPoint) -> Bool {
  return lhs.x == rhs.x  &&  lhs.y == rhs.y
}

public struct CGFloat {
#if arch(i386) || arch(arm)
  public typealias UnderlyingType = Float
#elseif arch(x86_64) || arch(arm64)
  public typealias UnderlyingType = Double
#endif

  public init() { 
    self.value = 0.0
  }

  public init(_ value: Int) { 
    self.value = UnderlyingType(value)
  }

  public init(_ value: Float) { 
    self.value = UnderlyingType(value)
  }

  public init(_ value: Double) { 
    self.value = UnderlyingType(value)
  }

  var value: UnderlyingType
}

public func ==(lhs: CGFloat, rhs: CGFloat) -> Bool {
  return lhs.value == rhs.value
}

extension CGFloat : ExpressibleByIntegerLiteral, ExpressibleByFloatLiteral, Equatable {
  public init(integerLiteral value: UnderlyingType) {
    self.value = value
  }

  public init(floatLiteral value: UnderlyingType) {
    self.value = value
  }
}

public extension Double {
  init(_ value: CGFloat) {
    self = Double(value.value)
  }
}
