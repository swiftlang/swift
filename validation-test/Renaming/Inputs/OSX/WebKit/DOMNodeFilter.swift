
var DOM_FILTER_ACCEPT: UInt32 { get }
var DOM_FILTER_REJECT: UInt32 { get }
var DOM_FILTER_SKIP: UInt32 { get }
var DOM_SHOW_ALL: UInt32 { get }
var DOM_SHOW_ELEMENT: UInt32 { get }
var DOM_SHOW_ATTRIBUTE: UInt32 { get }
var DOM_SHOW_TEXT: UInt32 { get }
var DOM_SHOW_CDATA_SECTION: UInt32 { get }
var DOM_SHOW_ENTITY_REFERENCE: UInt32 { get }
var DOM_SHOW_ENTITY: UInt32 { get }
var DOM_SHOW_PROCESSING_INSTRUCTION: UInt32 { get }
var DOM_SHOW_COMMENT: UInt32 { get }
var DOM_SHOW_DOCUMENT: UInt32 { get }
var DOM_SHOW_DOCUMENT_TYPE: UInt32 { get }
var DOM_SHOW_DOCUMENT_FRAGMENT: UInt32 { get }
var DOM_SHOW_NOTATION: UInt32 { get }
@available(OSX 10.4, *)
protocol DOMNodeFilter : NSObjectProtocol {
  @discardableResult
  func accept(_ n: DOMNode!) -> Int16
}
