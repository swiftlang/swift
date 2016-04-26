
typealias CGPDFStringRef = OpaquePointer
@available(iOS 2.0, *)
@discardableResult
func CGPDFStringGetLength(_ string: CGPDFStringRef?) -> Int
@available(iOS 2.0, *)
@discardableResult
func CGPDFStringGetBytePtr(_ string: CGPDFStringRef?) -> UnsafePointer<UInt8>?
@available(iOS 2.0, *)
@discardableResult
func CGPDFStringCopyTextString(_ string: CGPDFStringRef?) -> CFString?
@available(iOS 2.0, *)
@discardableResult
func CGPDFStringCopyDate(_ string: CGPDFStringRef?) -> CFDate?
