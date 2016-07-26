
@available(iOS 5.0, *)
class UIStepper : UIControl {
  var isContinuous: Bool
  var autorepeat: Bool
  var wraps: Bool
  var value: Double
  var minimumValue: Double
  var maximumValue: Double
  var stepValue: Double
  @available(iOS 6.0, *)
  func setBackgroundImage(_ image: UIImage?, for state: UIControlState)
  @available(iOS 6.0, *)
  @discardableResult
  func backgroundImage(for state: UIControlState) -> UIImage?
  @available(iOS 6.0, *)
  func setDividerImage(_ image: UIImage?, forLeftSegmentState leftState: UIControlState, rightSegmentState rightState: UIControlState)
  @available(iOS 6.0, *)
  @discardableResult
  func dividerImage(forLeftSegmentState state: UIControlState, rightSegmentState state: UIControlState) -> UIImage?
  @available(iOS 6.0, *)
  func setIncrementImage(_ image: UIImage?, for state: UIControlState)
  @available(iOS 6.0, *)
  @discardableResult
  func incrementImage(for state: UIControlState) -> UIImage?
  @available(iOS 6.0, *)
  func setDecrementImage(_ image: UIImage?, for state: UIControlState)
  @available(iOS 6.0, *)
  @discardableResult
  func decrementImage(for state: UIControlState) -> UIImage?
}
