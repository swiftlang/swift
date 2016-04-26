
class CFError {
}
@available(watchOS 2.0, *)
@discardableResult
func CFErrorGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
let kCFErrorDomainPOSIX: CFString!
@available(watchOS 2.0, *)
let kCFErrorDomainOSStatus: CFString!
@available(watchOS 2.0, *)
let kCFErrorDomainMach: CFString!
@available(watchOS 2.0, *)
let kCFErrorDomainCocoa: CFString!
@available(watchOS 2.0, *)
let kCFErrorLocalizedDescriptionKey: CFString!
@available(watchOS 2.0, *)
let kCFErrorLocalizedFailureReasonKey: CFString!
@available(watchOS 2.0, *)
let kCFErrorLocalizedRecoverySuggestionKey: CFString!
@available(watchOS 2.0, *)
let kCFErrorDescriptionKey: CFString!
@available(watchOS 2.0, *)
let kCFErrorUnderlyingErrorKey: CFString!
@available(watchOS 2.0, *)
let kCFErrorURLKey: CFString!
@available(watchOS 2.0, *)
let kCFErrorFilePathKey: CFString!
@available(watchOS 2.0, *)
@discardableResult
func CFErrorCreate(_ allocator: CFAllocator!, _ domain: CFString!, _ code: CFIndex, _ userInfo: CFDictionary!) -> CFError!
@available(watchOS 2.0, *)
@discardableResult
func CFErrorCreateWithUserInfoKeysAndValues(_ allocator: CFAllocator!, _ domain: CFString!, _ code: CFIndex, _ userInfoKeys: UnsafePointer<UnsafePointer<Void>?>!, _ userInfoValues: UnsafePointer<UnsafePointer<Void>?>!, _ numUserInfoValues: CFIndex) -> CFError!
@available(watchOS 2.0, *)
@discardableResult
func CFErrorGetDomain(_ err: CFError!) -> CFString!
@available(watchOS 2.0, *)
@discardableResult
func CFErrorGetCode(_ err: CFError!) -> CFIndex
@available(watchOS 2.0, *)
@discardableResult
func CFErrorCopyUserInfo(_ err: CFError!) -> CFDictionary!
@available(watchOS 2.0, *)
@discardableResult
func CFErrorCopyDescription(_ err: CFError!) -> CFString!
@available(watchOS 2.0, *)
@discardableResult
func CFErrorCopyFailureReason(_ err: CFError!) -> CFString!
@available(watchOS 2.0, *)
@discardableResult
func CFErrorCopyRecoverySuggestion(_ err: CFError!) -> CFString!
