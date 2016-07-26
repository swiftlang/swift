
@available(iOS 8.0, *)
class UIImageAsset : NSObject, NSSecureCoding {
  @discardableResult
  func image(with traitCollection: UITraitCollection) -> UIImage
  func register(_ image: UIImage, with traitCollection: UITraitCollection)
  func unregisterImage(with traitCollection: UITraitCollection)
}
