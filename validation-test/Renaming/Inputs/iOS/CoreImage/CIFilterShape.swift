
@available(iOS 9.0, *)
class CIFilterShape : NSObject, NSCopying {
  init(rect r: CGRect)
  @discardableResult
  func transform(by m: CGAffineTransform, interior flag: Bool) -> CIFilterShape
  @discardableResult
  func insetBy(x dx: Int32, y dy: Int32) -> CIFilterShape
  @discardableResult
  func union(with s2: CIFilterShape) -> CIFilterShape
  @discardableResult
  func union(with r: CGRect) -> CIFilterShape
  @discardableResult
  func intersect(with s2: CIFilterShape) -> CIFilterShape
  @discardableResult
  func intersect(with r: CGRect) -> CIFilterShape
  var extent: CGRect { get }
}
