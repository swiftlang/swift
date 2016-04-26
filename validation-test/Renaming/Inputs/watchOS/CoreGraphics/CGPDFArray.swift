
typealias CGPDFArrayRef = OpaquePointer
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetCount(_ array: CGPDFArrayRef?) -> Int
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetObject(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFObjectRef?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetNull(_ array: CGPDFArrayRef?, _ index: Int) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetBoolean(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFBoolean>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetInteger(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFInteger>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetNumber(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFReal>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetName(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<UnsafePointer<Int8>?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetString(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFStringRef?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetArray(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFArrayRef?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetDictionary(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFDictionaryRef?>?) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CGPDFArrayGetStream(_ array: CGPDFArrayRef?, _ index: Int, _ value: UnsafeMutablePointer<CGPDFStreamRef?>?) -> Bool
