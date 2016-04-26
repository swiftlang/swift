
typealias CGPDFArrayRef = OpaquePointer
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetCount(_ array: CGPDFArrayRef?) -> Int
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetObject(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFObjectRef?>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetNull(_ array: CGPDFArrayRef?, _ index: Int) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetBoolean(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFBoolean>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetInteger(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFInteger>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetNumber(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFReal>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetName(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<UnsafePointer<Int8>?>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetString(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFStringRef?>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetArray(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFArrayRef?>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetDictionary(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFDictionaryRef?>?) -> Bool
@available(OSX 10.3, *)
@discardableResult
func CGPDFArrayGetStream(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFStreamRef?>?) -> Bool
