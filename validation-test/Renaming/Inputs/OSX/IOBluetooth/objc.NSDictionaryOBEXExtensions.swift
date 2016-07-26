
extension NSMutableDictionary {
  convenience init!(obexHeadersData inHeadersData: UnsafePointer<Void>!, headersDataSize inDataSize: Int)
  convenience init!(obexHeadersData inHeadersData: NSData!)
  @discardableResult
  func getHeaderBytes() -> NSMutableData!
  @discardableResult
  func addTargetHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addHTTPHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addBodyHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32, endOfBody isEndOfBody: Bool) -> OBEXError
  @discardableResult
  func addWhoHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addConnectionIDHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addApplicationParameterHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addByteSequenceHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addObjectClassHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addAuthorizationChallengeHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addAuthorizationResponseHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addTimeISOHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addTypeHeader(_ type: String!) -> OBEXError
  @discardableResult
  func addLengthHeader(_ length: UInt32) -> OBEXError
  @discardableResult
  func addTime4ByteHeader(_ time4Byte: UInt32) -> OBEXError
  @discardableResult
  func addCountHeader(_ inCount: UInt32) -> OBEXError
  @discardableResult
  func addDescriptionHeader(_ inDescriptionString: String!) -> OBEXError
  @discardableResult
  func addNameHeader(_ inNameString: String!) -> OBEXError
  @discardableResult
  func addUserDefinedHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
  @discardableResult
  func addImageHandleHeader(_ type: String!) -> OBEXError
  @discardableResult
  func addImageDescriptorHeader(_ inHeaderData: UnsafePointer<Void>!, length inHeaderDataLength: UInt32) -> OBEXError
}
