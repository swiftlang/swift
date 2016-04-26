
@available(iOS 6.0, *)
class MKMapItem : NSObject {
  var placemark: MKPlacemark { get }
  var isCurrentLocation: Bool { get }
  var name: String?
  var phoneNumber: String?
  var url: NSURL?
  @available(iOS 9.0, *)
  @NSCopying var timeZone: NSTimeZone?
  @discardableResult
  class func forCurrentLocation() -> MKMapItem
  init(placemark placemark: MKPlacemark)
  @discardableResult
  func openInMaps(launchOptions launchOptions: [String : AnyObject]? = [:]) -> Bool
  @discardableResult
  class func openMaps(with mapItems: [MKMapItem], launchOptions launchOptions: [String : AnyObject]? = [:]) -> Bool
}
@available(iOS 6.0, *)
let MKLaunchOptionsDirectionsModeKey: String
@available(iOS 6.0, *)
let MKLaunchOptionsMapTypeKey: String
@available(iOS 6.0, *)
let MKLaunchOptionsShowsTrafficKey: String
@available(iOS 6.0, *)
let MKLaunchOptionsDirectionsModeDriving: String
@available(iOS 6.0, *)
let MKLaunchOptionsDirectionsModeWalking: String
@available(iOS 9.0, *)
let MKLaunchOptionsDirectionsModeTransit: String
@available(iOS 6.0, *)
let MKLaunchOptionsMapCenterKey: String
@available(iOS 6.0, *)
let MKLaunchOptionsMapSpanKey: String
@available(iOS 7.1, *)
let MKLaunchOptionsCameraKey: String
