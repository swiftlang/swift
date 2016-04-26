
enum NSPostingStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case postWhenIdle
  case postASAP
  case postNow
}
struct NSNotificationCoalescing : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var coalescingOnName: NSNotificationCoalescing { get }
  static var coalescingOnSender: NSNotificationCoalescing { get }
}
class NSNotificationQueue : NSObject {
  @discardableResult
  class func defaultQueue() -> NSNotificationQueue
  init(notificationCenter notificationCenter: NSNotificationCenter)
  func enqueue(_ notification: NSNotification, postingStyle postingStyle: NSPostingStyle)
  func enqueue(_ notification: NSNotification, postingStyle postingStyle: NSPostingStyle, coalesceMask coalesceMask: NSNotificationCoalescing, forModes modes: [String]?)
  func dequeueNotifications(matching notification: NSNotification, coalesceMask coalesceMask: Int)
}
