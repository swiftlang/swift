
class CARemoteLayerClient : NSObject {
  init(serverPort port: mach_port_t)
  func invalidate()
  var clientId: UInt32 { get }
  var layer: CALayer?
}
