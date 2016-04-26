
typealias CLGeocodeCompletionHandler = ([CLPlacemark]?, NSError?) -> Void
@available(iOS 5.0, *)
class CLGeocoder : NSObject {
  var isGeocoding: Bool { get }
  func reverseGeocodeLocation(_ location: CLLocation, completionHandler completionHandler: CLGeocodeCompletionHandler)
  func geocodeAddressDictionary(_ addressDictionary: [NSObject : AnyObject], completionHandler completionHandler: CLGeocodeCompletionHandler)
  func geocodeAddressString(_ addressString: String, completionHandler completionHandler: CLGeocodeCompletionHandler)
  func geocodeAddressString(_ addressString: String, in region: CLRegion?, completionHandler completionHandler: CLGeocodeCompletionHandler)
  func cancelGeocode()
}
