
@available(iOS 8.0, *)
enum NEVPNStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case invalid
  case disconnected
  case connecting
  case connected
  case reasserting
  case disconnecting
}
@available(iOS 8.0, *)
let NEVPNStatusDidChangeNotification: String
@available(iOS 9.0, *)
let NEVPNConnectionStartOptionUsername: String
@available(iOS 9.0, *)
let NEVPNConnectionStartOptionPassword: String
@available(iOS 8.0, *)
class NEVPNConnection : NSObject {
  @available(iOS 8.0, *)
  func startVPNTunnel() throws
  @available(iOS 9.0, *)
  func startVPNTunnel(options options: [String : NSObject]? = [:]) throws
  @available(iOS 8.0, *)
  func stopVPNTunnel()
  @available(iOS 8.0, *)
  var status: NEVPNStatus { get }
  @available(iOS 9.0, *)
  var connectedDate: NSDate? { get }
}
