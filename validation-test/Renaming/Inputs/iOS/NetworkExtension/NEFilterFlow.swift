
var NEFilterFlowBytesMax: UInt64 { get }
@available(iOS 9.0, *)
class NEFilterFlow : NSObject, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  var url: NSURL? { get }
}
@available(iOS 9.0, *)
class NEFilterBrowserFlow : NEFilterFlow, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  var request: NSURLRequest { get }
  @available(iOS 9.0, *)
  var response: NSURLResponse? { get }
  @available(iOS 9.0, *)
  var parentURL: NSURL? { get }
}
@available(iOS 9.0, *)
class NEFilterSocketFlow : NEFilterFlow, NSSecureCoding, NSCopying {
  @available(iOS 9.0, *)
  var remoteEndpoint: NWEndpoint { get }
  @available(iOS 9.0, *)
  var localEndpoint: NWEndpoint { get }
  @available(iOS 9.0, *)
  var socketFamily: Int32
  @available(iOS 9.0, *)
  var socketType: Int32
  @available(iOS 9.0, *)
  var socketProtocol: Int32
}
