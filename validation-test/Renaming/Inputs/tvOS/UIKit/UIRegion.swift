
@available(tvOS 9.0, *)
class UIRegion : NSObject, NSCopying, NSCoding {
  @discardableResult
  class func infinite() -> Self
  init(radius radius: CGFloat)
  init(size size: CGSize)
  @discardableResult
  func inverse() -> Self
  @discardableResult
  func byUnion(with region: UIRegion) -> Self
  @discardableResult
  func byDifference(from region: UIRegion) -> Self
  @discardableResult
  func byIntersection(with region: UIRegion) -> Self
  @discardableResult
  func contains(_ point: CGPoint) -> Bool
}
