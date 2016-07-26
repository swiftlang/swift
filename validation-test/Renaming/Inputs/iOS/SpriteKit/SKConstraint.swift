
@available(iOS 8.0, *)
class SKRange : NSObject, NSCoding, NSCopying {
  init(lowerLimit lower: CGFloat, upperLimit upper: CGFloat)
  convenience init(lowerLimit lower: CGFloat)
  convenience init(upperLimit upper: CGFloat)
  convenience init(constantValue value: CGFloat)
  convenience init(value value: CGFloat, variance variance: CGFloat)
  @discardableResult
  class func withNoLimits() -> Self
  var lowerLimit: CGFloat
  var upperLimit: CGFloat
}
@available(iOS 8.0, *)
class SKConstraint : NSObject, NSCoding, NSCopying {
  var enabled: Bool
  var referenceNode: SKNode?
  @discardableResult
  class func positionX(_ range: SKRange) -> Self
  @discardableResult
  class func positionY(_ range: SKRange) -> Self
  @discardableResult
  class func positionX(_ xRange: SKRange, y yRange: SKRange) -> Self
  @discardableResult
  class func distance(_ range: SKRange, to node: SKNode) -> Self
  @discardableResult
  class func distance(_ range: SKRange, to point: CGPoint) -> Self
  @discardableResult
  class func distance(_ range: SKRange, to point: CGPoint, in node: SKNode) -> Self
  @discardableResult
  class func zRotation(_ zRange: SKRange) -> Self
  @discardableResult
  class func orient(to node: SKNode, offset radians: SKRange) -> Self
  @discardableResult
  class func orient(to point: CGPoint, offset radians: SKRange) -> Self
  @discardableResult
  class func orient(to point: CGPoint, in node: SKNode, offset radians: SKRange) -> Self
}
