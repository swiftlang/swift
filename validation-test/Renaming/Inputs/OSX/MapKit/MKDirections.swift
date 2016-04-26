
typealias MKDirectionsHandler = (MKDirectionsResponse?, NSError?) -> Void
typealias MKETAHandler = (MKETAResponse?, NSError?) -> Void
@available(OSX 10.9, *)
class MKDirections : NSObject {
  init(request request: MKDirectionsRequest)
  func calculate(completionHandler completionHandler: MKDirectionsHandler)
  func calculateETA(completionHandler completionHandler: MKETAHandler)
  func cancel()
  var isCalculating: Bool { get }
}
