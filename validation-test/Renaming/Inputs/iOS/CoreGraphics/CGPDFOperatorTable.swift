
typealias CGPDFOperatorTableRef = OpaquePointer
typealias CGPDFOperatorCallback = @convention(c) (CGPDFScannerRef, UnsafeMutablePointer<Void>?) -> Void
@available(iOS 2.0, *)
@discardableResult
func CGPDFOperatorTableCreate() -> CGPDFOperatorTableRef?
@available(iOS 2.0, *)
@discardableResult
func CGPDFOperatorTableRetain(_ table: CGPDFOperatorTableRef?) -> CGPDFOperatorTableRef?
@available(iOS 2.0, *)
func CGPDFOperatorTableRelease(_ table: CGPDFOperatorTableRef?)
@available(iOS 2.0, *)
func CGPDFOperatorTableSetCallback(_ table: CGPDFOperatorTableRef?, _ name: UnsafePointer<Int8>?, _ callback: CGPDFOperatorCallback?)
