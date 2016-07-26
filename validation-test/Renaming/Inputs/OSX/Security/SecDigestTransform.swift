
let kSecDigestMD2: CFString
let kSecDigestMD4: CFString
let kSecDigestMD5: CFString
let kSecDigestSHA1: CFString
let kSecDigestSHA2: CFString
let kSecDigestHMACMD5: CFString
let kSecDigestHMACSHA1: CFString
let kSecDigestHMACSHA2: CFString
let kSecDigestTypeAttribute: CFString
let kSecDigestLengthAttribute: CFString
let kSecDigestHMACKeyAttribute: CFString
@available(OSX 10.7, *)
@discardableResult
func SecDigestTransformCreate(_ digestType: CFTypeRef?, _ digestLength: CFIndex, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecTransform
@available(OSX 10.7, *)
@discardableResult
func SecDigestTransformGetTypeID() -> CFTypeID
