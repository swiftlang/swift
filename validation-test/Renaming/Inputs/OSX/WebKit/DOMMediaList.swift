
@available(OSX 10.4, *)
class DOMMediaList : DOMObject {
  var mediaText: String!
  var length: UInt32 { get }
  @discardableResult
  func item(_ index: UInt32) -> String!
  func deleteMedium(_ oldMedium: String!)
  func appendMedium(_ newMedium: String!)
}
