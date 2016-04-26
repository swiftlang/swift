
@available(OSX 10.8, *)
class AVB17221ACMPMessage : NSObject, NSCopying {
  var messageType: AVB17221ACMPMessageType
  var status: AVB17221ACMPStatusCode
  var streamID: UInt64
  @available(OSX 10.9, *)
  var controllerEntityID: UInt64
  @available(OSX 10.9, *)
  var talkerEntityID: UInt64
  @available(OSX 10.9, *)
  var listenerEntityID: UInt64
  var talkerUniqueID: UInt16
  var listenerUniqueID: UInt16
  @NSCopying var destinationMAC: AVBMACAddress?
  var connectionCount: UInt16
  var sequenceID: UInt16
  var flags: AVB17221ACMPFlags
  var vlanID: UInt16
  @NSCopying var sourceMAC: AVBMACAddress?
}
