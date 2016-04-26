
@available(OSX 10.4, *)
class DOMCharacterData : DOMNode {
  var data: String!
  var length: UInt32 { get }
  @available(OSX 10.5, *)
  @discardableResult
  func substringData(_ offset: UInt32, length length: UInt32) -> String!
  func appendData(_ data: String!)
  @available(OSX 10.5, *)
  func insertData(_ offset: UInt32, data data: String!)
  @available(OSX 10.5, *)
  func deleteData(_ offset: UInt32, length length: UInt32)
  @available(OSX 10.5, *)
  func replaceData(_ offset: UInt32, length length: UInt32, data data: String!)
}
extension DOMCharacterData {
}
