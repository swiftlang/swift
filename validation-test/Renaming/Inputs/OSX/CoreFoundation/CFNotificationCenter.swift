
class CFNotificationCenter {
}
typealias CFNotificationCallback = @convention(c) (CFNotificationCenter!, UnsafeMutablePointer<Void>!, CFString!, UnsafePointer<Void>!, CFDictionary!) -> Void
enum CFNotificationSuspensionBehavior : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case drop
  case coalesce
  case hold
  case deliverImmediately
}
@discardableResult
func CFNotificationCenterGetTypeID() -> CFTypeID
@discardableResult
func CFNotificationCenterGetLocalCenter() -> CFNotificationCenter!
@discardableResult
func CFNotificationCenterGetDistributedCenter() -> CFNotificationCenter!
@discardableResult
func CFNotificationCenterGetDarwinNotifyCenter() -> CFNotificationCenter!
func CFNotificationCenterAddObserver(_ center: CFNotificationCenter!, _ observer: UnsafePointer<Void>!, _ callBack: CFNotificationCallback!, _ name: CFString!, _ object: UnsafePointer<Void>!, _ suspensionBehavior: CFNotificationSuspensionBehavior)
func CFNotificationCenterRemoveObserver(_ center: CFNotificationCenter!, _ observer: UnsafePointer<Void>!, _ name: CFString!, _ object: UnsafePointer<Void>!)
func CFNotificationCenterRemoveEveryObserver(_ center: CFNotificationCenter!, _ observer: UnsafePointer<Void>!)
func CFNotificationCenterPostNotification(_ center: CFNotificationCenter!, _ name: CFString!, _ object: UnsafePointer<Void>!, _ userInfo: CFDictionary!, _ deliverImmediately: Bool)
var kCFNotificationDeliverImmediately: CFOptionFlags { get }
var kCFNotificationPostToAllSessions: CFOptionFlags { get }
func CFNotificationCenterPostNotificationWithOptions(_ center: CFNotificationCenter!, _ name: CFString!, _ object: UnsafePointer<Void>!, _ userInfo: CFDictionary!, _ options: CFOptionFlags)
