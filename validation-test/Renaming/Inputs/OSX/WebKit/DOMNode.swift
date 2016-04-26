
var DOM_ELEMENT_NODE: Int { get }
var DOM_ATTRIBUTE_NODE: Int { get }
var DOM_TEXT_NODE: Int { get }
var DOM_CDATA_SECTION_NODE: Int { get }
var DOM_ENTITY_REFERENCE_NODE: Int { get }
var DOM_ENTITY_NODE: Int { get }
var DOM_PROCESSING_INSTRUCTION_NODE: Int { get }
var DOM_COMMENT_NODE: Int { get }
var DOM_DOCUMENT_NODE: Int { get }
var DOM_DOCUMENT_TYPE_NODE: Int { get }
var DOM_DOCUMENT_FRAGMENT_NODE: Int { get }
var DOM_NOTATION_NODE: Int { get }
var DOM_DOCUMENT_POSITION_DISCONNECTED: Int { get }
var DOM_DOCUMENT_POSITION_PRECEDING: Int { get }
var DOM_DOCUMENT_POSITION_FOLLOWING: Int { get }
var DOM_DOCUMENT_POSITION_CONTAINS: Int { get }
var DOM_DOCUMENT_POSITION_CONTAINED_BY: Int { get }
var DOM_DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: Int { get }
@available(OSX 10.4, *)
class DOMNode : DOMObject, DOMEventTarget {
  var nodeName: String! { get }
  var nodeValue: String!
  var nodeType: UInt16 { get }
  var parent: DOMNode! { get }
  var childNodes: DOMNodeList! { get }
  var firstChild: DOMNode! { get }
  var lastChild: DOMNode! { get }
  var previousSibling: DOMNode! { get }
  var nextSibling: DOMNode! { get }
  var ownerDocument: DOMDocument! { get }
  var namespaceURI: String! { get }
  var prefix: String!
  var localName: String! { get }
  var attributes: DOMNamedNodeMap! { get }
  @available(OSX 10.5, *)
  var baseURI: String! { get }
  @available(OSX 10.5, *)
  var textContent: String!
  @available(OSX 10.5, *)
  var parentElement: DOMElement! { get }
  @available(OSX 10.5, *)
  var isContentEditable: Bool { get }
  @available(OSX 10.5, *)
  @discardableResult
  func insert(before newChild: DOMNode!, refChild refChild: DOMNode!) -> DOMNode!
  @available(OSX 10.5, *)
  @discardableResult
  func replaceChild(_ newChild: DOMNode!, oldChild oldChild: DOMNode!) -> DOMNode!
  @discardableResult
  func removeChild(_ oldChild: DOMNode!) -> DOMNode!
  @discardableResult
  func appendChild(_ newChild: DOMNode!) -> DOMNode!
  @discardableResult
  func hasChildNodes() -> Bool
  @discardableResult
  func cloneNode(_ deep: Bool) -> DOMNode!
  func normalize()
  @available(OSX 10.5, *)
  @discardableResult
  func isSupported(_ feature: String!, version version: String!) -> Bool
  @discardableResult
  func hasAttributes() -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func isSameNode(_ other: DOMNode!) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func isEqualNode(_ other: DOMNode!) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func lookupPrefix(_ namespaceURI: String!) -> String!
  @available(OSX 10.5, *)
  @discardableResult
  func isDefaultNamespace(_ namespaceURI: String!) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  func lookupNamespaceURI(_ prefix: String!) -> String!
  @available(OSX 10.6, *)
  @discardableResult
  func compareDocumentPosition(_ other: DOMNode!) -> UInt16
  @available(OSX 10.5, *)
  @discardableResult
  func contains(_ other: DOMNode!) -> Bool
}
extension DOMNode {
}
