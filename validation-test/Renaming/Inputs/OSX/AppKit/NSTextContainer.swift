
@available(OSX 10.0, *)
class NSTextContainer : NSObject, NSCoding, NSTextLayoutOrientationProvider {
  @available(OSX 10.11, *)
  init(size size: NSSize)
  unowned(unsafe) var layoutManager: @sil_unmanaged NSLayoutManager?
  @available(OSX 10.0, *)
  func replaceLayoutManager(_ newLayoutManager: NSLayoutManager)
  @available(OSX 10.11, *)
  var size: NSSize
  @available(OSX 10.11, *)
  var exclusionPaths: [NSBezierPath]
  @available(OSX 10.11, *)
  var lineBreakMode: NSLineBreakMode
  var lineFragmentPadding: CGFloat
  @available(OSX 10.11, *)
  var maximumNumberOfLines: Int
  @available(OSX 10.11, *)
  @discardableResult
  func lineFragmentRect(forProposedRect proposedRect: NSRect, at characterIndex: Int, writingDirection baseWritingDirection: NSWritingDirection, remaining remainingRect: UnsafeMutablePointer<NSRect>?) -> NSRect
  @available(OSX 10.0, *)
  var isSimpleRectangularTextContainer: Bool { get }
  var widthTracksTextView: Bool
  var heightTracksTextView: Bool
  var textView: NSTextView?
}
struct __tcFlags {
  var widthTracksTextView: UInt16
  var heightTracksTextView: UInt16
  var observingFrameChanges: UInt16
  var lineBreakMode: UInt16
  var oldAPI: UInt16
  var _reserved: UInt16
  init()
  init(widthTracksTextView widthTracksTextView: UInt16, heightTracksTextView heightTracksTextView: UInt16, observingFrameChanges observingFrameChanges: UInt16, lineBreakMode lineBreakMode: UInt16, oldAPI oldAPI: UInt16, _reserved _reserved: UInt16)
}
enum NSLineSweepDirection : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case left
  case right
  case down
  case up
}
enum NSLineMovementDirection : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case doesntMove
  case movesLeft
  case movesRight
  case movesDown
  case movesUp
}
extension NSTextContainer {
  convenience init(containerSize aContainerSize: NSSize)
  var containerSize: NSSize
  @discardableResult
  func lineFragmentRect(forProposedRect proposedRect: NSRect, sweepDirection sweepDirection: NSLineSweepDirection, movementDirection movementDirection: NSLineMovementDirection, remaining remainingRect: NSRectPointer?) -> NSRect
  @available(OSX, introduced: 10.0, deprecated: 10.11)
  @discardableResult
  func contains(_ point: NSPoint) -> Bool
}
