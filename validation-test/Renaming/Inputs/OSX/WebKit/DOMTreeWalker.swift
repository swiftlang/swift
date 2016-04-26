
@available(OSX 10.4, *)
class DOMTreeWalker : DOMObject {
  var root: DOMNode! { get }
  var whatToShow: UInt32 { get }
  var filter: DOMNodeFilter! { get }
  var expandEntityReferences: Bool { get }
  var currentNode: DOMNode!
  @discardableResult
  func parentNode() -> DOMNode!
  @discardableResult
  func firstChild() -> DOMNode!
  @discardableResult
  func lastChild() -> DOMNode!
  @discardableResult
  func previousSibling() -> DOMNode!
  @discardableResult
  func nextSibling() -> DOMNode!
  @discardableResult
  func previousNode() -> DOMNode!
  @discardableResult
  func nextNode() -> DOMNode!
}
