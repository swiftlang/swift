
let GCControllerDidConnectNotification: String
let GCControllerDidDisconnectNotification: String
enum GCControllerPlayerIndex : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case indexUnset
  case index1
  case index2
  case index3
  case index4
}
@available(OSX 10.9, *)
class GCController : NSObject {
  var controllerPausedHandler: ((GCController) -> Void)?
  @available(OSX 10.11, *)
  var handlerQueue: dispatch_queue_t
  var vendorName: String? { get }
  var isAttachedToDevice: Bool { get }
  var playerIndex: GCControllerPlayerIndex
  var gamepad: GCGamepad? { get }
  var extendedGamepad: GCExtendedGamepad? { get }
  @available(OSX 10.10, *)
  var motion: GCMotion? { get }
  @discardableResult
  class func controllers() -> [GCController]
  class func startWirelessControllerDiscovery(completionHandler completionHandler: (() -> Void)? = nil)
  class func stopWirelessControllerDiscovery()
}
