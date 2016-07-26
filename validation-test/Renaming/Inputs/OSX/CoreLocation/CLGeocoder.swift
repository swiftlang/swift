
typealias CLGeocodeCompletionHandler = ([CLPlacemark]?, NSError?) -> Void
@available(OSX 10.8, *)
class CLGeocoder : NSObject {
  var isGeocoding: Bool { get }
  func reverseGeocodeLocation(_ location: CLLocation, completionHandler completionHandler: CLGeocodeCompletionHandler)
  func geocodeAddressDictionary(_ addressDictionary: [NSObject : AnyObject], completionHandler completionHandler: CLGeocodeCompletionHandler)
  func geocodeAddressString(_ addressString: String, completionHandler completionHandler: CLGeocodeCompletionHandler)
  func geocodeAddressString(_ addressString: String, in region: CLRegion?, completionHandler completionHandler: CLGeocodeCompletionHandler)
  func cancelGeocode()
}
