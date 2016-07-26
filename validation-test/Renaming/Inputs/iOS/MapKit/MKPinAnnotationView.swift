
@available(iOS, introduced: 3.0, deprecated: 9.0, message: "Use MKPinAnnotationView's pinTintColor instead")
enum MKPinAnnotationColor : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case red
  case green
  case purple
}
@available(iOS 3.0, *)
class MKPinAnnotationView : MKAnnotationView {
  @available(iOS 9.0, *)
  @discardableResult
  class func redPinColor() -> UIColor
  @available(iOS 9.0, *)
  @discardableResult
  class func greenPinColor() -> UIColor
  @available(iOS 9.0, *)
  @discardableResult
  class func purplePinColor() -> UIColor
  @available(iOS 9.0, *)
  var pinTintColor: UIColor!
  var animatesDrop: Bool
  @available(iOS, introduced: 3.0, deprecated: 9.0, message: "Use pinTintColor instead")
  var pinColor: MKPinAnnotationColor
}
