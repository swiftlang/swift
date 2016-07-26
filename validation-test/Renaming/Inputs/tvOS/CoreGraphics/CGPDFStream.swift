
typealias CGPDFStreamRef = OpaquePointer
enum CGPDFDataFormat : Int32 {
  init?(rawValue rawValue: Int32)
  var rawValue: Int32 { get }
  case raw
  case jpegEncoded
  case JPEG2000
}
@available(tvOS 2.0, *)
@discardableResult
func CGPDFStreamGetDictionary(_ stream: CGPDFStreamRef?) -> CGPDFDictionaryRef?
@available(tvOS 2.0, *)
@discardableResult
func CGPDFStreamCopyData(_ stream: CGPDFStreamRef?, _ format: UnsafeMutablePointer<CGPDFDataFormat>?) -> CFData?
