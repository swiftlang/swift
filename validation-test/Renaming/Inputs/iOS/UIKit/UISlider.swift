
@available(iOS 2.0, *)
class UISlider : UIControl, NSCoding {
  var value: Float
  var minimumValue: Float
  var maximumValue: Float
  var minimumValueImage: UIImage?
  var maximumValueImage: UIImage?
  var isContinuous: Bool
  @available(iOS 5.0, *)
  var minimumTrackTintColor: UIColor?
  @available(iOS 5.0, *)
  var maximumTrackTintColor: UIColor?
  @available(iOS 5.0, *)
  var thumbTintColor: UIColor?
  func setValue(_ value: Float, animated animated: Bool)
  func setThumbImage(_ image: UIImage?, for state: UIControlState)
  func setMinimumTrackImage(_ image: UIImage?, for state: UIControlState)
  func setMaximumTrackImage(_ image: UIImage?, for state: UIControlState)
  @discardableResult
  func thumbImage(for state: UIControlState) -> UIImage?
  @discardableResult
  func minimumTrackImage(for state: UIControlState) -> UIImage?
  @discardableResult
  func maximumTrackImage(for state: UIControlState) -> UIImage?
  var currentThumbImage: UIImage? { get }
  var currentMinimumTrackImage: UIImage? { get }
  var currentMaximumTrackImage: UIImage? { get }
  @discardableResult
  func minimumValueImageRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func maximumValueImageRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func trackRect(forBounds bounds: CGRect) -> CGRect
  @discardableResult
  func thumbRect(forBounds bounds: CGRect, trackRect rect: CGRect, value value: Float) -> CGRect
}
