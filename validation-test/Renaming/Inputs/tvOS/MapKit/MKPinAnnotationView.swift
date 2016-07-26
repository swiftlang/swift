
@available(tvOS 9.2, *)
class MKPinAnnotationView : MKAnnotationView {
  @available(tvOS 9.0, *)
  @discardableResult
  class func redPinColor() -> UIColor
  @available(tvOS 9.0, *)
  @discardableResult
  class func greenPinColor() -> UIColor
  @available(tvOS 9.0, *)
  @discardableResult
  class func purplePinColor() -> UIColor
  @available(tvOS 9.0, *)
  var pinTintColor: UIColor!
  var animatesDrop: Bool
}
