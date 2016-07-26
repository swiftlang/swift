
typealias VTSession = CFTypeRef
@available(OSX 10.8, *)
@discardableResult
func VTSessionCopySupportedPropertyDictionary(_ session: VTSession, _ supportedPropertyDictionaryOut: UnsafeMutablePointer<CFDictionary?>) -> OSStatus
@available(OSX 10.8, *)
let kVTPropertyTypeKey: CFString
@available(OSX 10.8, *)
let kVTPropertyType_Enumeration: CFString
@available(OSX 10.8, *)
let kVTPropertyType_Boolean: CFString
@available(OSX 10.8, *)
let kVTPropertyType_Number: CFString
@available(OSX 10.8, *)
let kVTPropertyReadWriteStatusKey: CFString
@available(OSX 10.8, *)
let kVTPropertyReadWriteStatus_ReadOnly: CFString
@available(OSX 10.8, *)
let kVTPropertyReadWriteStatus_ReadWrite: CFString
@available(OSX 10.8, *)
let kVTPropertyShouldBeSerializedKey: CFString
@available(OSX 10.8, *)
let kVTPropertySupportedValueMinimumKey: CFString
@available(OSX 10.8, *)
let kVTPropertySupportedValueMaximumKey: CFString
@available(OSX 10.8, *)
let kVTPropertySupportedValueListKey: CFString
@available(OSX 10.8, *)
let kVTPropertyDocumentationKey: CFString
@available(OSX 10.8, *)
@discardableResult
func VTSessionSetProperty(_ session: VTSession, _ propertyKey: CFString, _ propertyValue: CFTypeRef) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTSessionCopyProperty(_ session: VTSession, _ propertyKey: CFString, _ allocator: CFAllocator?, _ propertyValueOut: UnsafeMutablePointer<Void>?) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTSessionSetProperties(_ session: VTSession, _ propertyDictionary: CFDictionary) -> OSStatus
@available(OSX 10.8, *)
@discardableResult
func VTSessionCopySerializableProperties(_ session: VTSession, _ allocator: CFAllocator?, _ dictionaryOut: UnsafeMutablePointer<CFDictionary?>) -> OSStatus
