
@available(OSX 10.4, *)
class DOMHTMLCollection : DOMObject {
  var length: UInt32 { get }
  @discardableResult
  func item(_ index: UInt32) -> DOMNode!
  @discardableResult
  func namedItem(_ name: String!) -> DOMNode!
  @available(OSX 10.6, *)
  @discardableResult
  func tags(_ name: String!) -> DOMNodeList!
}
