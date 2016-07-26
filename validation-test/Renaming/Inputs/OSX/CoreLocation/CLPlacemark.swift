
@available(OSX 10.8, *)
class CLPlacemark : NSObject, NSCopying, NSSecureCoding {
  init(placemark placemark: CLPlacemark)
  @NSCopying var location: CLLocation? { get }
  @NSCopying var region: CLRegion? { get }
  @available(OSX 10.11, *)
  @NSCopying var timeZone: NSTimeZone? { get }
  var addressDictionary: [NSObject : AnyObject]? { get }
  var name: String? { get }
  var thoroughfare: String? { get }
  var subThoroughfare: String? { get }
  var locality: String? { get }
  var subLocality: String? { get }
  var administrativeArea: String? { get }
  var subAdministrativeArea: String? { get }
  var postalCode: String? { get }
  var isOcountryCode: String? { get }
  var country: String? { get }
  var inlandWater: String? { get }
  var ocean: String? { get }
  var areasOfInterest: [String]? { get }
}
