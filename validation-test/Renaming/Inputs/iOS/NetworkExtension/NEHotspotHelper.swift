
@available(iOS 9.0, *)
enum NEHotspotHelperCommandType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case filterScanList
  case evaluate
  case authenticate
  case presentUI
  case maintain
  case logoff
}
@available(iOS 9.0, *)
enum NEHotspotHelperResult : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case success
  case failure
  case uiRequired
  case commandNotRecognized
  case authenticationRequired
  case unsupportedNetwork
  case temporaryFailure
}
@available(iOS 9.0, *)
enum NEHotspotHelperConfidence : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case low
  case high
}
@available(iOS 9.0, *)
class NEHotspotNetwork : NSObject {
  @available(iOS 9.0, *)
  var ssid: String { get }
  @available(iOS 9.0, *)
  var bssid: String { get }
  @available(iOS 9.0, *)
  var signalStrength: Double { get }
  @available(iOS 9.0, *)
  var isSecure: Bool { get }
  @available(iOS 9.0, *)
  var didAutoJoin: Bool { get }
  @available(iOS 9.0, *)
  var didJustJoin: Bool { get }
  @available(iOS 9.0, *)
  var isChosenHelper: Bool { get }
  @available(iOS 9.0, *)
  func setConfidence(_ confidence: NEHotspotHelperConfidence)
  @available(iOS 9.0, *)
  func setPassword(_ password: String)
}
@available(iOS 9.0, *)
class NEHotspotHelperCommand : NSObject {
  @available(iOS 9.0, *)
  var commandType: NEHotspotHelperCommandType { get }
  @available(iOS 9.0, *)
  var network: NEHotspotNetwork? { get }
  @available(iOS 9.0, *)
  var networkList: [NEHotspotNetwork]? { get }
  @available(iOS 9.0, *)
  @discardableResult
  func createResponse(_ result: NEHotspotHelperResult) -> NEHotspotHelperResponse
  @available(iOS 9.0, *)
  @discardableResult
  func createTCPConnection(_ endpoint: NWEndpoint) -> NWTCPConnection
  @available(iOS 9.0, *)
  @discardableResult
  func createUDPSession(_ endpoint: NWEndpoint) -> NWUDPSession
}
@available(iOS 9.0, *)
class NEHotspotHelperResponse : NSObject {
  @available(iOS 9.0, *)
  func setNetwork(_ network: NEHotspotNetwork)
  @available(iOS 9.0, *)
  func setNetworkList(_ networkList: [NEHotspotNetwork])
  @available(iOS 9.0, *)
  func deliver()
}
@available(iOS 9.0, *)
typealias NEHotspotHelperHandler = (NEHotspotHelperCommand) -> Void
@available(iOS 9.0, *)
let kNEHotspotHelperOptionDisplayName: String
@available(iOS 9.0, *)
class NEHotspotHelper : NSObject {
  @available(iOS 9.0, *)
  @discardableResult
  class func register(options options: [String : NSObject]? = [:], queue queue: dispatch_queue_t, handler handler: NEHotspotHelperHandler) -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  class func logoff(_ network: NEHotspotNetwork) -> Bool
  @available(iOS 9.0, *)
  @discardableResult
  class func supportedNetworkInterfaces() -> [AnyObject]
}
extension NSMutableURLRequest {
  @available(iOS 9.0, *)
  func bind(to command: NEHotspotHelperCommand)
}
