
typealias CGPDFOperatorTableRef = OpaquePointer
typealias CGPDFOperatorCallback = @convention(c) (CGPDFScannerRef, UnsafeMutablePointer<Void>?) -> Void
@available(OSX 10.4, *)
@discardableResult
func CGPDFOperatorTableCreate() -> CGPDFOperatorTableRef?
@available(OSX 10.4, *)
@discardableResult
func CGPDFOperatorTableRetain(_ table: CGPDFOperatorTableRef?) -> CGPDFOperatorTableRef?
@available(OSX 10.4, *)
func CGPDFOperatorTableRelease(_ table: CGPDFOperatorTableRef?)
@available(OSX 10.4, *)
func CGPDFOperatorTableSetCallback(_ table: CGPDFOperatorTableRef?, _ name: UnsafePointer<Int8>?, _ callback: CGPDFOperatorCallback?)
