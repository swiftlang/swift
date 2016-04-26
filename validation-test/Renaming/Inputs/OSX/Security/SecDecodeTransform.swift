
let kSecDecodeTypeAttribute: CFString
@available(OSX 10.7, *)
@discardableResult
func SecDecodeTransformCreate(_ DecodeType: CFTypeRef, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecTransform?
