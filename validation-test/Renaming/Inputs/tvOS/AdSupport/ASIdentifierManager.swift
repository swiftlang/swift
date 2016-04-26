
@available(tvOS 6.0, *)
class ASIdentifierManager : NSObject {
  @discardableResult
  class func shared() -> ASIdentifierManager!
  var advertisingIdentifier: NSUUID! { get }
  var isAdvertisingTrackingEnabled: Bool { get }
}
