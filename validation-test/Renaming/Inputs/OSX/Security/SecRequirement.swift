
@discardableResult
func SecRequirementGetTypeID() -> CFTypeID
@discardableResult
func SecRequirementCreateWithData(_ data: CFData, _ flags: SecCSFlags, _ requirement: UnsafeMutablePointer<SecRequirement?>) -> OSStatus
@discardableResult
func SecRequirementCreateWithString(_ text: CFString, _ flags: SecCSFlags, _ requirement: UnsafeMutablePointer<SecRequirement?>) -> OSStatus
@discardableResult
func SecRequirementCreateWithStringAndErrors(_ text: CFString, _ flags: SecCSFlags, _ errors: UnsafeMutablePointer<Unmanaged<CFError>?>?, _ requirement: UnsafeMutablePointer<SecRequirement?>) -> OSStatus
@discardableResult
func SecRequirementCopyData(_ requirement: SecRequirement, _ flags: SecCSFlags, _ data: UnsafeMutablePointer<CFData?>) -> OSStatus
@discardableResult
func SecRequirementCopyString(_ requirement: SecRequirement, _ flags: SecCSFlags, _ text: UnsafeMutablePointer<CFString?>) -> OSStatus
