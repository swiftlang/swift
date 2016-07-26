
@available(OSX 10.4, *)
let DOMEventException: String
@available(OSX 10.4, *)
struct DOMEventExceptionCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var DOM_UNSPECIFIED_EVENT_TYPE_ERR: DOMEventExceptionCode { get }
