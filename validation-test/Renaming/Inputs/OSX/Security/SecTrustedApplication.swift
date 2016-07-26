
@discardableResult
func SecTrustedApplicationGetTypeID() -> CFTypeID
@discardableResult
func SecTrustedApplicationCreateFromPath(_ path: UnsafePointer<Int8>?, _ app: UnsafeMutablePointer<SecTrustedApplication?>) -> OSStatus
@discardableResult
func SecTrustedApplicationCopyData(_ appRef: SecTrustedApplication, _ data: UnsafeMutablePointer<CFData?>) -> OSStatus
@discardableResult
func SecTrustedApplicationSetData(_ appRef: SecTrustedApplication, _ data: CFData) -> OSStatus
