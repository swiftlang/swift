
@available(OSX 10.8, *)
class AVB17221EntityDiscovery : NSObject {
  var interfaceName: String
  unowned(unsafe) var interface: @sil_unmanaged AVBInterface? { get }
  unowned(unsafe) var discoveryDelegate: @sil_unmanaged AVB17221EntityDiscoveryDelegate?
  init(interfaceName anInterfaceName: String)
  func primeIterators()
  @discardableResult
  func discoverEntities() -> Bool
  @discardableResult
  func discoverEntity(_ entityID: UInt64) -> Bool
  func addLocalEntity(_ anEntity: AVB17221Entity) throws
  func removeLocalEntity(_ guid: UInt64) throws
  @available(OSX 10.9, *)
  func changeEntity(withEntityID entityID: UInt64, toNewGPTPGrandmasterID gPTPGrandmasterID: UInt64) throws
}
