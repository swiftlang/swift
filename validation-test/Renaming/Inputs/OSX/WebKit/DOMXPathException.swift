
@available(OSX 10.4, *)
let DOMXPathException: String
@available(OSX 10.4, *)
struct DOMXPathExceptionCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var DOM_INVALID_EXPRESSION_ERR: DOMXPathExceptionCode { get }
var DOM_TYPE_ERR: DOMXPathExceptionCode { get }
