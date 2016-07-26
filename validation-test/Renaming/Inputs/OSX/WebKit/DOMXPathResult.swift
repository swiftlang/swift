
var DOM_ANY_TYPE: Int { get }
var DOM_NUMBER_TYPE: Int { get }
var DOM_STRING_TYPE: Int { get }
var DOM_BOOLEAN_TYPE: Int { get }
var DOM_UNORDERED_NODE_ITERATOR_TYPE: Int { get }
var DOM_ORDERED_NODE_ITERATOR_TYPE: Int { get }
var DOM_UNORDERED_NODE_SNAPSHOT_TYPE: Int { get }
var DOM_ORDERED_NODE_SNAPSHOT_TYPE: Int { get }
var DOM_ANY_UNORDERED_NODE_TYPE: Int { get }
var DOM_FIRST_ORDERED_NODE_TYPE: Int { get }
@available(OSX 10.5, *)
class DOMXPathResult : DOMObject {
  var resultType: UInt16 { get }
  var numberValue: Double { get }
  var stringValue: String! { get }
  var booleanValue: Bool { get }
  var singleNodeValue: DOMNode! { get }
  var invalidIteratorState: Bool { get }
  var snapshotLength: UInt32 { get }
  @discardableResult
  func iterateNext() -> DOMNode!
  @discardableResult
  func snapshotItem(_ index: UInt32) -> DOMNode!
}
