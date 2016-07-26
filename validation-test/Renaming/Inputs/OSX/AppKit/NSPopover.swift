
@available(OSX, introduced: 10.7, deprecated: 10.10)
enum NSPopoverAppearance : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  @available(OSX, introduced: 10.7, deprecated: 10.10)
  case minimal
  @available(OSX, introduced: 10.7, deprecated: 10.10)
  case HUD
}
enum NSPopoverBehavior : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case applicationDefined
  case transient
  case semitransient
}
@available(OSX 10.7, *)
class NSPopover : NSResponder, NSAppearanceCustomization, NSAccessibilityElementProtocol, NSAccessibility {
  @IBOutlet unowned(unsafe) var delegate: @sil_unmanaged NSPopoverDelegate?
  var behavior: NSPopoverBehavior
  var animates: Bool
  @IBOutlet var contentViewController: NSViewController?
  var contentSize: NSSize
  var isShown: Bool { get }
  @available(OSX 10.10, *)
  var isDetached: Bool { get }
  var positioningRect: NSRect
  func showRelative(to positioningRect: NSRect, of positioningView: NSView, preferredEdge preferredEdge: NSRectEdge)
  @IBAction func performClose(_ sender: AnyObject?)
  func close()
}
@available(OSX 10.7, *)
let NSPopoverCloseReasonKey: String
@available(OSX 10.7, *)
let NSPopoverCloseReasonStandard: String
@available(OSX 10.7, *)
let NSPopoverCloseReasonDetachToWindow: String
@available(OSX 10.7, *)
let NSPopoverWillShowNotification: String
@available(OSX 10.7, *)
let NSPopoverDidShowNotification: String
@available(OSX 10.7, *)
let NSPopoverWillCloseNotification: String
@available(OSX 10.7, *)
let NSPopoverDidCloseNotification: String
protocol NSPopoverDelegate : NSObjectProtocol {
  @available(OSX 10.7, *)
  @discardableResult
  optional func popoverShouldClose(_ popover: NSPopover) -> Bool
  @available(OSX 10.10, *)
  @discardableResult
  optional func popoverShouldDetach(_ popover: NSPopover) -> Bool
  @available(OSX 10.10, *)
  optional func popoverDidDetach(_ popover: NSPopover)
  @available(OSX 10.7, *)
  @discardableResult
  optional func detachableWindow(for popover: NSPopover) -> NSWindow?
  optional func popoverWillShow(_ notification: NSNotification)
  optional func popoverDidShow(_ notification: NSNotification)
  optional func popoverWillClose(_ notification: NSNotification)
  optional func popoverDidClose(_ notification: NSNotification)
}
