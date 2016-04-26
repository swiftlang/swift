
typealias CGPDFScannerRef = OpaquePointer
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerCreate(_ cs: CGPDFContentStreamRef, _ table: CGPDFOperatorTableRef?, _ info: UnsafeMutablePointer<Void>?) -> CGPDFScannerRef
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerRetain(_ scanner: CGPDFScannerRef?) -> CGPDFScannerRef?
@available(OSX 10.4, *)
func CGPDFScannerRelease(_ scanner: CGPDFScannerRef?)
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerScan(_ scanner: CGPDFScannerRef?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerGetContentStream(_ scanner: CGPDFScannerRef) -> CGPDFContentStreamRef
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopObject(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFObjectRef?>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopBoolean(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFBoolean>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopInteger(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFInteger>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopNumber(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFReal>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopName(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<UnsafePointer<Int8>?>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopString(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFStringRef?>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopArray(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFArrayRef?>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopDictionary(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFDictionaryRef?>?) -> Bool
@available(OSX 10.4, *)
@discardableResult
func CGPDFScannerPopStream(_ scanner: CGPDFScannerRef, _ value: UnsafeMutablePointer<CGPDFStreamRef?>?) -> Bool
