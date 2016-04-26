
@available(tvOS 9.2, *)
class MKMapItem : NSObject {
  var placemark: MKPlacemark { get }
  var isCurrentLocation: Bool { get }
  var name: String?
  var phoneNumber: String?
  var url: NSURL?
  @available(tvOS 9.0, *)
  @NSCopying var timeZone: NSTimeZone?
  @discardableResult
  class func forCurrentLocation() -> MKMapItem
  init(placemark placemark: MKPlacemark)
}
