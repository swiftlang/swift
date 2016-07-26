
class CFUserNotification {
}
typealias CFUserNotificationCallBack = @convention(c) (CFUserNotification!, CFOptionFlags) -> Void
@discardableResult
func CFUserNotificationGetTypeID() -> CFTypeID
@discardableResult
func CFUserNotificationCreate(_ allocator: CFAllocator!, _ timeout: CFTimeInterval, _ flags: CFOptionFlags, _ error: UnsafeMutablePointer<Int32>!, _ dictionary: CFDictionary!) -> CFUserNotification!
@discardableResult
func CFUserNotificationReceiveResponse(_ userNotification: CFUserNotification!, _ timeout: CFTimeInterval, _ responseFlags: UnsafeMutablePointer<CFOptionFlags>!) -> Int32
@discardableResult
func CFUserNotificationGetResponseValue(_ userNotification: CFUserNotification!, _ key: CFString!, _ idx: CFIndex) -> CFString!
@discardableResult
func CFUserNotificationGetResponseDictionary(_ userNotification: CFUserNotification!) -> CFDictionary!
@discardableResult
func CFUserNotificationUpdate(_ userNotification: CFUserNotification!, _ timeout: CFTimeInterval, _ flags: CFOptionFlags, _ dictionary: CFDictionary!) -> Int32
@discardableResult
func CFUserNotificationCancel(_ userNotification: CFUserNotification!) -> Int32
@discardableResult
func CFUserNotificationCreateRunLoopSource(_ allocator: CFAllocator!, _ userNotification: CFUserNotification!, _ callout: CFUserNotificationCallBack!, _ order: CFIndex) -> CFRunLoopSource!
@discardableResult
func CFUserNotificationDisplayNotice(_ timeout: CFTimeInterval, _ flags: CFOptionFlags, _ iconURL: CFURL!, _ soundURL: CFURL!, _ localizationURL: CFURL!, _ alertHeader: CFString!, _ alertMessage: CFString!, _ defaultButtonTitle: CFString!) -> Int32
@discardableResult
func CFUserNotificationDisplayAlert(_ timeout: CFTimeInterval, _ flags: CFOptionFlags, _ iconURL: CFURL!, _ soundURL: CFURL!, _ localizationURL: CFURL!, _ alertHeader: CFString!, _ alertMessage: CFString!, _ defaultButtonTitle: CFString!, _ alternateButtonTitle: CFString!, _ otherButtonTitle: CFString!, _ responseFlags: UnsafeMutablePointer<CFOptionFlags>!) -> Int32
var kCFUserNotificationStopAlertLevel: CFOptionFlags { get }
var kCFUserNotificationNoteAlertLevel: CFOptionFlags { get }
var kCFUserNotificationCautionAlertLevel: CFOptionFlags { get }
var kCFUserNotificationPlainAlertLevel: CFOptionFlags { get }
var kCFUserNotificationDefaultResponse: CFOptionFlags { get }
var kCFUserNotificationAlternateResponse: CFOptionFlags { get }
var kCFUserNotificationOtherResponse: CFOptionFlags { get }
var kCFUserNotificationCancelResponse: CFOptionFlags { get }
var kCFUserNotificationNoDefaultButtonFlag: CFOptionFlags { get }
var kCFUserNotificationUseRadioButtonsFlag: CFOptionFlags { get }
@discardableResult
func CFUserNotificationCheckBoxChecked(_ i: CFIndex) -> CFOptionFlags
@discardableResult
func CFUserNotificationSecureTextField(_ i: CFIndex) -> CFOptionFlags
@discardableResult
func CFUserNotificationPopUpSelection(_ n: CFIndex) -> CFOptionFlags
let kCFUserNotificationIconURLKey: CFString!
let kCFUserNotificationSoundURLKey: CFString!
let kCFUserNotificationLocalizationURLKey: CFString!
let kCFUserNotificationAlertHeaderKey: CFString!
let kCFUserNotificationAlertMessageKey: CFString!
let kCFUserNotificationDefaultButtonTitleKey: CFString!
let kCFUserNotificationAlternateButtonTitleKey: CFString!
let kCFUserNotificationOtherButtonTitleKey: CFString!
let kCFUserNotificationProgressIndicatorValueKey: CFString!
let kCFUserNotificationPopUpTitlesKey: CFString!
let kCFUserNotificationTextFieldTitlesKey: CFString!
let kCFUserNotificationCheckBoxTitlesKey: CFString!
let kCFUserNotificationTextFieldValuesKey: CFString!
@available(OSX 10.3, *)
let kCFUserNotificationPopUpSelectionKey: CFString!
