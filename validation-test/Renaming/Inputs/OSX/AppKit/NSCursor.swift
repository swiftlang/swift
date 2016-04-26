
var NSAppKitVersionNumberWithCursorSizeSupport: Double { get }
class NSCursor : NSObject, NSCoding {
  @discardableResult
  class func current() -> NSCursor
  @available(OSX 10.6, *)
  @discardableResult
  class func currentSystem() -> NSCursor?
  @discardableResult
  class func arrow() -> NSCursor
  @discardableResult
  class func iBeam() -> NSCursor
  @discardableResult
  class func pointingHand() -> NSCursor
  @discardableResult
  class func closedHand() -> NSCursor
  @discardableResult
  class func openHand() -> NSCursor
  @discardableResult
  class func resizeLeft() -> NSCursor
  @discardableResult
  class func resizeRight() -> NSCursor
  @discardableResult
  class func resizeLeftRight() -> NSCursor
  @discardableResult
  class func resizeUp() -> NSCursor
  @discardableResult
  class func resizeDown() -> NSCursor
  @discardableResult
  class func resizeUpDown() -> NSCursor
  @discardableResult
  class func crosshair() -> NSCursor
  @discardableResult
  class func disappearingItem() -> NSCursor
  @available(OSX 10.5, *)
  @discardableResult
  class func operationNotAllowed() -> NSCursor
  @available(OSX 10.6, *)
  @discardableResult
  class func dragLink() -> NSCursor
  @available(OSX 10.6, *)
  @discardableResult
  class func dragCopy() -> NSCursor
  @available(OSX 10.6, *)
  @discardableResult
  class func contextualMenu() -> NSCursor
  @available(OSX 10.7, *)
  @discardableResult
  class func iBeamCursorForVerticalLayout() -> NSCursor
  init(image newImage: NSImage, hotSpot aPoint: NSPoint)
  convenience init(image newImage: NSImage, foregroundColorHint fg: NSColor?, backgroundColorHint bg: NSColor?, hotSpot hotSpot: NSPoint)
  class func hide()
  class func unhide()
  class func setHiddenUntilMouseMoves(_ flag: Bool)
  class func pop()
  var image: NSImage { get }
  var hotSpot: NSPoint { get }
  func push()
  func pop()
  func set()
  func setOnMouseExited(_ flag: Bool)
  func setOnMouseEntered(_ flag: Bool)
  var isSetOnMouseExited: Bool { get }
  var isSetOnMouseEntered: Bool { get }
  func mouseEntered(_ theEvent: NSEvent)
  func mouseExited(_ theEvent: NSEvent)
}

extension NSCursor : CustomPlaygroundQuickLookable {
}
struct _cursorFlags {
  var onMouseExited: UInt32
  var onMouseEntered: UInt32
  var cursorType: UInt32
  init()
  init(onMouseExited onMouseExited: UInt32, onMouseEntered onMouseEntered: UInt32, cursorType cursorType: UInt32)
}
