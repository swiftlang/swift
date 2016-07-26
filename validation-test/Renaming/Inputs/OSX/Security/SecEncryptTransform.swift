
let kSecPaddingNoneKey: CFString
let kSecPaddingPKCS1Key: CFString
let kSecPaddingPKCS5Key: CFString
let kSecPaddingPKCS7Key: CFString
@available(OSX 10.8, *)
let kSecPaddingOAEPKey: CFString
let kSecModeNoneKey: CFString
let kSecModeECBKey: CFString
let kSecModeCBCKey: CFString
let kSecModeCFBKey: CFString
let kSecModeOFBKey: CFString
let kSecEncryptKey: CFString
let kSecPaddingKey: CFString
let kSecIVKey: CFString
let kSecEncryptionMode: CFString
@available(OSX 10.8, *)
let kSecOAEPMessageLengthAttributeName: CFString
@available(OSX 10.8, *)
let kSecOAEPEncodingParametersAttributeName: CFString
@available(OSX 10.8, *)
let kSecOAEPMGF1DigestAlgorithmAttributeName: CFString
@available(OSX 10.7, *)
@discardableResult
func SecEncryptTransformCreate(_ keyRef: SecKey, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecTransform
@available(OSX 10.7, *)
@discardableResult
func SecDecryptTransformCreate(_ keyRef: SecKey, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecTransform
@available(OSX 10.7, *)
@discardableResult
func SecDecryptTransformGetTypeID() -> CFTypeID
@available(OSX 10.7, *)
@discardableResult
func SecEncryptTransformGetTypeID() -> CFTypeID
