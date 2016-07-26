
typealias CGPDFDictionaryRef = OpaquePointer
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetCount(_ dict: CGPDFDictionaryRef?) -> Int
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetObject(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFObjectRef?>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetBoolean(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFBoolean>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetInteger(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFInteger>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetNumber(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFReal>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetName(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<UnsafePointer<Int8>?>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetString(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFStringRef?>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetArray(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFArrayRef?>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetDictionary(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFDictionaryRef?>?) -> Bool
@available(tvOS 2.0, *)
@discardableResult
func CGPDFDictionaryGetStream(_ dict: CGPDFDictionaryRef?, _ key: UnsafePointer<Int8>, _ value: UnsafeMutablePointer<CGPDFStreamRef?>?) -> Bool
typealias CGPDFDictionaryApplierFunction = @convention(c) (UnsafePointer<Int8>, CGPDFObjectRef, UnsafeMutablePointer<Void>?) -> Void
@available(tvOS 2.0, *)
func CGPDFDictionaryApplyFunction(_ dict: CGPDFDictionaryRef?, _ function: CGPDFDictionaryApplierFunction?, _ info: UnsafeMutablePointer<Void>?)
