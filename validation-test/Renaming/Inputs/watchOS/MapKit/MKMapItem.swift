
@available(watchOS 2.0, *)
class MKMapItem : NSObject {
  var placemark: MKPlacemark { get }
  var isCurrentLocation: Bool { get }
  var name: String?
  var phoneNumber: String?
  var url: NSURL?
  @available(watchOS 2.0, *)
  @NSCopying var timeZone: NSTimeZone?
  @discardableResult
  class func forCurrentLocation() -> MKMapItem
  init(placemark placemark: MKPlacemark)
  @discardableResult
  func openInMaps(launchOptions launchOptions: [String : AnyObject]? = [:]) -> Bool
  @discardableResult
  class func openMaps(with mapItems: [MKMapItem], launchOptions launchOptions: [String : AnyObject]? = [:]) -> Bool
}
@available(watchOS 2.0, *)
let MKLaunchOptionsDirectionsModeKey: String
@available(watchOS 2.0, *)
let MKLaunchOptionsDirectionsModeDriving: String
@available(watchOS 2.0, *)
let MKLaunchOptionsDirectionsModeWalking: String
@available(watchOS 2.0, *)
let MKLaunchOptionsDirectionsModeTransit: String
@available(watchOS 2.0, *)
let MKLaunchOptionsMapCenterKey: String
@available(watchOS 2.0, *)
let MKLaunchOptionsMapSpanKey: String
@available(watchOS 2.0, *)
let MKLaunchOptionsCameraKey: String
