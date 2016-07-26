
typealias CGPDFScannerRef = OpaquePointer
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerCreate(_ cs: CGPDFContentStreamRef, _ table: CGPDFOperatorTableRef?, _ info: UnsafeMutablePointer<Void>?) -> CGPDFScannerRef
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerRetain(_ scanner: CGPDFScannerRef?) -> CGPDFScannerRef?
@available(iOS 2.0, *)
func CGPDFScannerRelease(_ scanner: CGPDFScannerRef?)
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerScan(_ scanner: CGPDFScannerRef?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerGetContentStream(_ scanner: CGPDFScannerRef) -> CGPDFContentStreamRef
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopObject(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFObjectRef?>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopBoolean(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFBoolean>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopInteger(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFInteger>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopNumber(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFReal>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopName(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<UnsafePointer<Int8>?>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopString(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFStringRef?>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopArray(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFArrayRef?>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopDictionary(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFDictionaryRef?>?) -> Bool
@available(iOS 2.0, *)
@discardableResult
func CGPDFScannerPopStream(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFStreamRef?>?) -> Bool
