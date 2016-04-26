
var DOM_START_TO_START: Int { get }
var DOM_START_TO_END: Int { get }
var DOM_END_TO_END: Int { get }
var DOM_END_TO_START: Int { get }
var DOM_NODE_BEFORE: Int { get }
var DOM_NODE_AFTER: Int { get }
var DOM_NODE_BEFORE_AND_AFTER: Int { get }
var DOM_NODE_INSIDE: Int { get }
@available(OSX 10.4, *)
class DOMRange : DOMObject {
  var startContainer: DOMNode! { get }
  var startOffset: Int32 { get }
  var endContainer: DOMNode! { get }
  var endOffset: Int32 { get }
  var collapsed: Bool { get }
  var commonAncestorContainer: DOMNode! { get }
  @available(OSX 10.5, *)
  var text: String! { get }
  @available(OSX 10.5, *)
  func setStart(_ refNode: DOMNode!, offset offset: Int32)
  @available(OSX 10.5, *)
  func setEnd(_ refNode: DOMNode!, offset offset: Int32)
  func setStartBefore(_ refNode: DOMNode!)
  func setStartAfter(_ refNode: DOMNode!)
  func setEndBefore(_ refNode: DOMNode!)
  func setEndAfter(_ refNode: DOMNode!)
  func collapse(_ toStart: Bool)
  func select(_ refNode: DOMNode!)
  func selectNodeContents(_ refNode: DOMNode!)
  @available(OSX 10.5, *)
  @discardableResult
  func compareBoundaryPoints(_ how: UInt16, sourceRange sourceRange: DOMRange!) -> Int16
  func deleteContents()
  @discardableResult
  func extractContents() -> DOMDocumentFragment!
  @discardableResult
  func cloneContents() -> DOMDocumentFragment!
  func insert(_ newNode: DOMNode!)
  func surroundContents(_ newParent: DOMNode!)
  @discardableResult
  func clone() -> DOMRange!
  @discardableResult
  func toString() -> String!
  func detach()
  @available(OSX 10.5, *)
  @discardableResult
  func createContextualFragment(_ html: String!) -> DOMDocumentFragment!
  @available(OSX 10.5, *)
  @discardableResult
  func intersects(_ refNode: DOMNode!) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func compare(_ refNode: DOMNode!) -> Int16
  @available(OSX 10.5, *)
  @discardableResult
  func comparePoint(_ refNode: DOMNode!, offset offset: Int32) -> Int16
  @available(OSX 10.5, *)
  @discardableResult
  func isPoint(inRange refNode: DOMNode!, offset offset: Int32) -> Bool
}
extension DOMRange {
}
