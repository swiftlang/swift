
@available(OSX 10.8, *)
class AVB17221Entity : NSObject {
  var isLocalEntity: Bool
  var timeToLive: UInt8
  @available(OSX 10.9, *)
  var entityID: UInt64
  @available(OSX 10.9, *)
  var entityModelID: UInt64
  var entityCapabilities: AVB17221ADPEntityCapabilities
  var talkerStreamSources: UInt16
  var talkerCapabilities: AVB17221ADPTalkerCapabilities
  var listenerStreamSinks: UInt16
  var listenerCapabilities: AVB17221ADPListenerCapabilities
  var controllerCapabilities: AVB17221ADPControllerCapabilities
  var availableIndex: UInt32
  @available(OSX 10.9, *)
  var gPTPGrandmasterID: UInt64
  @available(OSX 10.9, *)
  var gPTPDomainNumber: UInt8
  @available(OSX 10.9, *)
  var identifyControlIndex: UInt16
  @available(OSX 10.9, *)
  var interfaceIndex: UInt16
  var associationID: UInt64
  var macAddresses: [AVBMACAddress]
  unowned(unsafe) var entityDiscovery: @sil_unmanaged AVB17221EntityDiscovery?
}
