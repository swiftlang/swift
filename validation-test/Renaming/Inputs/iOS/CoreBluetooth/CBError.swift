
let CBErrorDomain: String
enum CBError : Int {
  case unknown
  @available(iOS 6.0, *)
  case invalidParameters
  @available(iOS 6.0, *)
  case invalidHandle
  @available(iOS 6.0, *)
  case notConnected
  @available(iOS 6.0, *)
  case outOfSpace
  @available(iOS 6.0, *)
  case operationCancelled
  @available(iOS 6.0, *)
  case connectionTimeout
  @available(iOS 6.0, *)
  case peripheralDisconnected
  @available(iOS 6.0, *)
  case uuidNotAllowed
  @available(iOS 6.0, *)
  case alreadyAdvertising
  @available(iOS 7.1, *)
  case connectionFailed
  @available(iOS 9.0, *)
  case connectionLimitReached
}

extension CBError : _BridgedNSError {
}
let CBATTErrorDomain: String
enum CBATTError : Int {
  @available(iOS 6.0, *)
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
