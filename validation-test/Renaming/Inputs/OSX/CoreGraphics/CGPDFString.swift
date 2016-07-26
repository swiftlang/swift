
typealias CGPDFStringRef = OpaquePointer
@available(OSX 10.3, *)
@discardableResult
func CGPDFStringGetLength(_ string: CGPDFStringRef?) -> Int
@available(OSX 10.3, *)
@discardableResult
func CGPDFStringGetBytePtr(_ string: CGPDFStringRef?) -> UnsafePointer<UInt8>?
@available(OSX 10.3, *)
@discardableResult
func CGPDFStringCopyTextString(_ string: CGPDFStringRef?) -> CFString?
@available(OSX 10.4, *)
@discardableResult
func CGPDFStringCopyDate(_ string: CGPDFStringRef?) -> CFDate?
