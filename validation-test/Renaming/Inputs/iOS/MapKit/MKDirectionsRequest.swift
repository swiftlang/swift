
@available(iOS 6.0, *)
class MKDirectionsRequest : NSObject {
  var source: MKMapItem?
  var destination: MKMapItem?
}
extension MKDirectionsRequest {
  @available(iOS 7.0, *)
  var transportType: MKDirectionsTransportType
  @available(iOS 7.0, *)
  var requestsAlternateRoutes: Bool
  @available(iOS 7.0, *)
  @NSCopying var departureDate: NSDate?
  @available(iOS 7.0, *)
  @NSCopying var arrivalDate: NSDate?
}
extension MKDirectionsRequest {
  @available(iOS 6.0, *)
  init(contentsOf url: NSURL)
  @available(iOS 6.0, *)
  @discardableResult
  class func isDirectionsRequest(_ url: NSURL) -> Bool
}
