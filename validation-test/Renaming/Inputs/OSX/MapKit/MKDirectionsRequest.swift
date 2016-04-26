
@available(OSX 10.9, *)
class MKDirectionsRequest : NSObject {
  var source: MKMapItem?
  var destination: MKMapItem?
}
extension MKDirectionsRequest {
  @available(OSX 10.9, *)
  var transportType: MKDirectionsTransportType
  @available(OSX 10.9, *)
  var requestsAlternateRoutes: Bool
  @available(OSX 10.9, *)
  @NSCopying var departureDate: NSDate?
  @available(OSX 10.9, *)
  @NSCopying var arrivalDate: NSDate?
}
extension MKDirectionsRequest {
  @available(OSX 10.9, *)
  init(contentsOf url: NSURL)
  @available(OSX 10.9, *)
  @discardableResult
  class func isDirectionsRequest(_ url: NSURL) -> Bool
}
