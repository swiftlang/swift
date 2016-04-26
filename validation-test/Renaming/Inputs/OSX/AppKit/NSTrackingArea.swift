
struct NSTrackingAreaOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var mouseEnteredAndExited: NSTrackingAreaOptions { get }
  static var mouseMoved: NSTrackingAreaOptions { get }
  static var cursorUpdate: NSTrackingAreaOptions { get }
  static var activeWhenFirstResponder: NSTrackingAreaOptions { get }
  static var activeInKeyWindow: NSTrackingAreaOptions { get }
  static var activeInActiveApp: NSTrackingAreaOptions { get }
  static var activeAlways: NSTrackingAreaOptions { get }
  static var assumeInside: NSTrackingAreaOptions { get }
  static var inVisibleRect: NSTrackingAreaOptions { get }
  static var enabledDuringMouseDrag: NSTrackingAreaOptions { get }
}
@available(OSX 10.5, *)
class NSTrackingArea : NSObject, NSCopying, NSCoding {
  init(rect rect: NSRect, options options: NSTrackingAreaOptions = [], owner owner: AnyObject?, userInfo userInfo: [NSObject : AnyObject]? = [:])
  var rect: NSRect { get }
  var options: NSTrackingAreaOptions { get }
  unowned(unsafe) var owner: @sil_unmanaged AnyObject? { get }
  var userInfo: [NSObject : AnyObject]? { get }
}
