
@available(iOS 3.0, *)
class MKPlacemark : CLPlacemark, MKAnnotation {
  init(coordinate coordinate: CLLocationCoordinate2D, addressDictionary addressDictionary: [String : AnyObject]?)
  var countryCode: String? { get }
}
