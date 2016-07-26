
@available(tvOS 9.2, *)
class MKDirectionsRequest : NSObject {
  var source: MKMapItem?
  var destination: MKMapItem?
}
extension MKDirectionsRequest {
  @available(tvOS 7.0, *)
  var transportType: MKDirectionsTransportType
  @available(tvOS 7.0, *)
  var requestsAlternateRoutes: Bool
  @available(tvOS 7.0, *)
  @NSCopying var departureDate: NSDate?
  @available(tvOS 7.0, *)
  @NSCopying var arrivalDate: NSDate?
}
extension MKDirectionsRequest {
  @available(tvOS 6.0, *)
  init(contentsOf url: NSURL)
  @available(tvOS 6.0, *)
  @discardableResult
  class func isDirectionsRequest(_ url: NSURL) -> Bool
}
