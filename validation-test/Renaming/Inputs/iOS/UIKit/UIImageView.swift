
@available(iOS 2.0, *)
class UIImageView : UIView {
  init(image image: UIImage?)
  @available(iOS 3.0, *)
  init(image image: UIImage?, highlightedImage highlightedImage: UIImage?)
  var image: UIImage?
  @available(iOS 3.0, *)
  var highlightedImage: UIImage?
  @available(iOS 3.0, *)
  var isHighlighted: Bool
  var animationImages: [UIImage]?
  @available(iOS 3.0, *)
  var highlightedAnimationImages: [UIImage]?
  var animationDuration: NSTimeInterval
  var animationRepeatCount: Int
  func startAnimating()
  func stopAnimating()
  @discardableResult
  func isAnimating() -> Bool
}
