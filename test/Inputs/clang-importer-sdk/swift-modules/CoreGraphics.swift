@exported import ObjectiveC
@exported import CoreGraphics

@public func == (lhs: CGPoint, rhs: CGPoint) -> Bool {
  return lhs.x == rhs.x  &&  lhs.y == rhs.y
}
