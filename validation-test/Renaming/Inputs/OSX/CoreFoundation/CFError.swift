
class CFError {
}
@available(OSX 10.5, *)
@discardableResult
func CFErrorGetTypeID() -> CFTypeID
@available(OSX 10.5, *)
let kCFErrorDomainPOSIX: CFString!
@available(OSX 10.5, *)
let kCFErrorDomainOSStatus: CFString!
@available(OSX 10.5, *)
let kCFErrorDomainMach: CFString!
@available(OSX 10.5, *)
let kCFErrorDomainCocoa: CFString!
@available(OSX 10.5, *)
let kCFErrorLocalizedDescriptionKey: CFString!
@available(OSX 10.5, *)
let kCFErrorLocalizedFailureReasonKey: CFString!
@available(OSX 10.5, *)
let kCFErrorLocalizedRecoverySuggestionKey: CFString!
@available(OSX 10.5, *)
let kCFErrorDescriptionKey: CFString!
@available(OSX 10.5, *)
let kCFErrorUnderlyingErrorKey: CFString!
@available(OSX 10.7, *)
let kCFErrorURLKey: CFString!
@available(OSX 10.7, *)
let kCFErrorFilePathKey: CFString!
@available(OSX 10.5, *)
@discardableResult
func CFErrorCreate(_ allocator: CFAllocator!, _ domain: CFString!, _ code: CFIndex, _ userInfo: CFDictionary!) -> CFError!
@available(OSX 10.5, *)
@discardableResult
func CFErrorCreateWithUserInfoKeysAndValues(_ allocator: CFAllocator!, _ domain: CFString!, _ code: CFIndex, _ userInfoKeys: UnsafePointer<UnsafePointer<Void>?>!, _ userInfoValues: UnsafePointer<UnsafePointer<Void>?>!, _ numUserInfoValues: CFIndex) -> CFError!
@available(OSX 10.5, *)
@discardableResult
func CFErrorGetDomain(_ err: CFError!) -> CFString!
@available(OSX 10.5, *)
@discardableResult
func CFErrorGetCode(_ err: CFError!) -> CFIndex
@available(OSX 10.5, *)
@discardableResult
func CFErrorCopyUserInfo(_ err: CFError!) -> CFDictionary!
@available(OSX 10.5, *)
@discardableResult
func CFErrorCopyDescription(_ err: CFError!) -> CFString!
@available(OSX 10.5, *)
@discardableResult
func CFErrorCopyFailureReason(_ err: CFError!) -> CFString!
@available(OSX 10.5, *)
@discardableResult
func CFErrorCopyRecoverySuggestion(_ err: CFError!) -> CFString!
