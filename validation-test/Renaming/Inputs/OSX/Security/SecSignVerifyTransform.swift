
let kSecKeyAttributeName: CFString
let kSecSignatureAttributeName: CFString
let kSecInputIsAttributeName: CFString
let kSecInputIsPlainText: CFString
let kSecInputIsDigest: CFString
let kSecInputIsRaw: CFString
@available(OSX 10.7, *)
@discardableResult
func SecSignTransformCreate(_ key: SecKey, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecTransform?
@available(OSX 10.7, *)
@discardableResult
func SecVerifyTransformCreate(_ key: SecKey, _ signature: CFData?, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecTransform?
