
let AVBNullEUI64: UInt64
@available(OSX 10.10, *)
class AVBCentralManager : NSObject {
  func startControllerMatching()
  func didAdd(_ interface: AVBInterface)
  func didRemove(_ interface: AVBInterface)
  @discardableResult
  func streamingEnabledInterfacesOnly() -> Bool
  @discardableResult
  class func nextAvailableDynamicEntityID() -> UInt64
  class func releaseDynamicEntityID(_ entityID: UInt64)
  @discardableResult
  class func nextAvailableDynamicEntityModelID() -> UInt64
  class func releaseDynamicEntityModelID(_ entityModelID: UInt64)
}
