
let CBErrorDomain: String
enum CBError : Int {
  case unknown
  @available(OSX 10.9, *)
  case invalidParameters
  @available(OSX 10.9, *)
  case invalidHandle
  @available(OSX 10.9, *)
  case notConnected
  @available(OSX 10.9, *)
  case outOfSpace
  @available(OSX 10.9, *)
  case operationCancelled
  @available(OSX 10.9, *)
  case connectionTimeout
  @available(OSX 10.9, *)
  case peripheralDisconnected
  @available(OSX 10.9, *)
  case uuidNotAllowed
  @available(OSX 10.9, *)
  case alreadyAdvertising
  @available(OSX 10.11, *)
  case maxConnection
}

extension CBError : _BridgedNSError {
}
let CBATTErrorDomain: String
enum CBATTError : Int {
  @available(OSX 10.9, *)
  case success
  case invalidHandle
  case readNotPermitted
  case writeNotPermitted
  case invalidPdu
  case insufficientAuthentication
  case requestNotSupported
  case invalidOffset
  case insufficientAuthorization
  case prepareQueueFull
  case attributeNotFound
  case attributeNotLong
  case insufficientEncryptionKeySize
  case invalidAttributeValueLength
  case unlikelyError
  case insufficientEncryption
  case unsupportedGroupType
  case insufficientResources
}

extension CBATTError : _BridgedNSError {
}
