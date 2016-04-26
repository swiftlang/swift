
@available(OSX 10.4, *)
class DOMNamedNodeMap : DOMObject {
  var length: UInt32 { get }
  @discardableResult
  func getNamedItem(_ name: String!) -> DOMNode!
  @discardableResult
  func setNamedItem(_ node: DOMNode!) -> DOMNode!
  @discardableResult
  func removeNamedItem(_ name: String!) -> DOMNode!
  @discardableResult
  func item(_ index: UInt32) -> DOMNode!
  @available(OSX 10.5, *)
  @discardableResult
  func getNamedItemNS(_ namespaceURI: String!, localName localName: String!) -> DOMNode!
  @discardableResult
  func setNamedItemNS(_ node: DOMNode!) -> DOMNode!
  @available(OSX 10.5, *)
  @discardableResult
  func removeNamedItemNS(_ namespaceURI: String!, localName localName: String!) -> DOMNode!
}
extension DOMNamedNodeMap {
}
