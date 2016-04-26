
class DRNotificationCenterRef {
}
typealias DRNotificationCallback = @convention(c) (DRNotificationCenterRef!, UnsafeMutablePointer<Void>!, CFString!, DRType!, CFDictionary!) -> Void
@available(OSX 10.2, *)
@discardableResult
func DRNotificationCenterGetTypeID() -> CFTypeID
@available(OSX 10.2, *)
@discardableResult
func DRNotificationCenterCreate() -> Unmanaged<DRNotificationCenterRef>!
@available(OSX 10.2, *)
@discardableResult
func DRNotificationCenterCreateRunLoopSource(_ center: DRNotificationCenterRef!) -> Unmanaged<CFRunLoopSource>!
@available(OSX 10.2, *)
func DRNotificationCenterAddObserver(_ center: DRNotificationCenterRef!, _ observer: UnsafePointer<Void>!, _ callback: DRNotificationCallback!, _ name: CFString!, _ object: DRType!)
@available(OSX 10.2, *)
func DRNotificationCenterRemoveObserver(_ center: DRNotificationCenterRef!, _ observer: UnsafePointer<Void>!, _ name: CFString!, _ object: DRType!)
