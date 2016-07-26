
typealias CGPDFOperatorTableRef = OpaquePointer
typealias CGPDFOperatorCallback = @convention(c) (CGPDFScannerRef, UnsafeMutablePointer<Void>?) -> Void
@available(tvOS 2.0, *)
@discardableResult
func CGPDFOperatorTableCreate() -> CGPDFOperatorTableRef?
@available(tvOS 2.0, *)
@discardableResult
func CGPDFOperatorTableRetain(_ table: CGPDFOperatorTableRef?) -> CGPDFOperatorTableRef?
@available(tvOS 2.0, *)
func CGPDFOperatorTableRelease(_ table: CGPDFOperatorTableRef?)
@available(tvOS 2.0, *)
func CGPDFOperatorTableSetCallback(_ table: CGPDFOperatorTableRef?, _ name: UnsafePointer<Int8>?, _ callback: CGPDFOperatorCallback?)
