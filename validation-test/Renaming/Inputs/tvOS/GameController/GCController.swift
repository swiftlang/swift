
let GCControllerDidConnectNotification: String
let GCControllerDidDisconnectNotification: String
@available(tvOS 9.0, *)
class GCEventViewController : UIViewController {
  var controllerUserInteractionEnabled: Bool
}
enum GCControllerPlayerIndex : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case indexUnset
  case index1
  case index2
  case index3
  case index4
}
@available(tvOS 7.0, *)
class GCController : NSObject {
  var controllerPausedHandler: ((GCController) -> Void)?
  var handlerQueue: dispatch_queue_t
  var vendorName: String? { get }
  var isAttachedToDevice: Bool { get }
  var playerIndex: GCControllerPlayerIndex
  var gamepad: GCGamepad? { get }
  var microGamepad: GCMicroGamepad? { get }
  var extendedGamepad: GCExtendedGamepad? { get }
  @available(tvOS 8.0, *)
  var motion: GCMotion? { get }
  @discardableResult
  class func controllers() -> [GCController]
  class func startWirelessControllerDiscovery(completionHandler completionHandler: (() -> Void)? = nil)
  class func stopWirelessControllerDiscovery()
}
