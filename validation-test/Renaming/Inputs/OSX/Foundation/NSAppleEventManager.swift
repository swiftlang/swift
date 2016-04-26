
typealias NSAppleEventManagerSuspensionID = OpaquePointer
let NSAppleEventTimeOutDefault: Double
let NSAppleEventTimeOutNone: Double
let NSAppleEventManagerWillProcessFirstEventNotification: String
class NSAppleEventManager : NSObject {
  @discardableResult
  class func shared() -> NSAppleEventManager
  func setEventHandler(_ handler: AnyObject, andSelector handleEventSelector: Selector, forEventClass eventClass: AEEventClass, andEventID eventID: AEEventID)
  func removeEventHandler(forEventClass eventClass: AEEventClass, andEventID eventID: AEEventID)
  @discardableResult
  func dispatchRawAppleEvent(_ theAppleEvent: UnsafePointer<AppleEvent>, withRawReply theReply: UnsafeMutablePointer<AppleEvent>, handlerRefCon handlerRefCon: SRefCon) -> OSErr
  var currentAppleEvent: NSAppleEventDescriptor? { get }
  var currentReplyAppleEvent: NSAppleEventDescriptor? { get }
  @discardableResult
  func suspendCurrentAppleEvent() -> NSAppleEventManagerSuspensionID?
  @discardableResult
  func appleEvent(forSuspensionID suspensionID: NSAppleEventManagerSuspensionID) -> NSAppleEventDescriptor
  @discardableResult
  func replyAppleEvent(forSuspensionID suspensionID: NSAppleEventManagerSuspensionID) -> NSAppleEventDescriptor
  func setCurrentAppleEventAndReplyEventWithSuspensionID(_ suspensionID: NSAppleEventManagerSuspensionID)
  func resume(withSuspensionID suspensionID: NSAppleEventManagerSuspensionID)
}
