
@available(iOS 8.0, *)
class CLVisit : NSObject, NSSecureCoding, NSCopying {
  @NSCopying var arrivalDate: NSDate { get }
  @NSCopying var departureDate: NSDate { get }
  var coordinate: CLLocationCoordinate2D { get }
  var horizontalAccuracy: CLLocationAccuracy { get }
}
