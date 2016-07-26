
@available(OSX 10.10, *)
class SKRegion : NSObject, NSCopying, NSCoding {
  var path: CGPath? { get }
  @discardableResult
  class func infinite() -> Self
  init(radius radius: Float)
  init(size size: CGSize)
  init(path path: CGPath)
  @discardableResult
  func inverse() -> Self
  @discardableResult
  func byUnion(with region: SKRegion) -> Self
  @discardableResult
  func byDifference(from region: SKRegion) -> Self
  @discardableResult
  func byIntersection(with region: SKRegion) -> Self
  @discardableResult
  func contains(_ point: CGPoint) -> Bool
}
