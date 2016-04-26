
@available(OSX 10.4, *)
let DOMRangeException: String
@available(OSX 10.4, *)
struct DOMRangeExceptionCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var DOM_BAD_BOUNDARYPOINTS_ERR: DOMRangeExceptionCode { get }
var DOM_INVALID_NODE_TYPE_ERR: DOMRangeExceptionCode { get }
