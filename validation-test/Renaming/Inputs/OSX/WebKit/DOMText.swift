
@available(OSX 10.4, *)
class DOMText : DOMCharacterData {
  @available(OSX 10.6, *)
  var wholeText: String! { get }
  @discardableResult
  func splitText(_ offset: UInt32) -> DOMText!
  @available(OSX 10.6, *)
  @discardableResult
  func replaceWholeText(_ content: String!) -> DOMText!
}
