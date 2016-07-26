
let CBErrorDomain: String
enum CBError : Int {
  case unknown
  @available(tvOS 6.0, *)
  case invalidParameters
  @available(tvOS 6.0, *)
  case invalidHandle
  @available(tvOS 6.0, *)
  case notConnected
  @available(tvOS 6.0, *)
  case outOfSpace
  @available(tvOS 6.0, *)
  case operationCancelled
  @available(tvOS 6.0, *)
  case connectionTimeout
  @available(tvOS 6.0, *)
  case peripheralDisconnected
  @available(tvOS 6.0, *)
  case uuidNotAllowed
  @available(tvOS 6.0, *)
  case alreadyAdvertising
  @available(tvOS 7.1, *)
  case connectionFailed
  @available(tvOS 9.0, *)
  case connectionLimitReached
}

extension CBError : _BridgedNSError {
}
let CBATTErrorDomain: String
enum CBATTError : Int {
  @available(tvOS 6.0, *)
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
