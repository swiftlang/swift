
struct NSDragOperation : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var copy: NSDragOperation { get }
  static var link: NSDragOperation { get }
  static var generic: NSDragOperation { get }
  static var `private`: NSDragOperation { get }
  static var move: NSDragOperation { get }
  static var delete: NSDragOperation { get }
  static var every: NSDragOperation { get }
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  static var all_Obsolete: NSDragOperation { get }
  @available(OSX, introduced: 10.0, deprecated: 10.10)
  static var all: NSDragOperation { get }
}
@available(OSX 10.7, *)
enum NSDraggingFormation : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
  case none
  case pile
  case list
  case stack
}
@available(OSX 10.7, *)
enum NSDraggingContext : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case outsideApplication
  case withinApplication
}
@available(OSX 10.7, *)
struct NSDraggingItemEnumerationOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var concurrent: NSDraggingItemEnumerationOptions { get }
  static var clearNonenumeratedImages: NSDraggingItemEnumerationOptions { get }
}
@available(OSX 10.11, *)
enum NSSpringLoadingHighlight : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case standard
  case emphasized
}
protocol NSDraggingInfo : NSObjectProtocol {
  @discardableResult
  func draggingDestinationWindow() -> NSWindow?
  @discardableResult
  func draggingSourceOperationMask() -> NSDragOperation
  @discardableResult
  func draggingLocation() -> NSPoint
  @discardableResult
  func draggedImageLocation() -> NSPoint
  @discardableResult
  func draggedImage() -> NSImage?
  @discardableResult
  func draggingPasteboard() -> NSPasteboard
  @discardableResult
  func draggingSource() -> AnyObject?
  @discardableResult
  func draggingSequenceNumber() -> Int
  func slideDraggedImage(to screenPoint: NSPoint)
  @discardableResult
  func namesOfPromisedFilesDropped(atDestination dropDestination: NSURL) -> [String]?
  @available(OSX 10.7, *)
  var draggingFormation: NSDraggingFormation { get set }
  @available(OSX 10.7, *)
  var animatesToDestination: Bool { get set }
  @available(OSX 10.7, *)
  var numberOfValidItemsForDrop: Int { get set }
  @available(OSX 10.7, *)
  func enumerateDraggingItems(_ enumOpts: NSDraggingItemEnumerationOptions = [], for view: NSView, classes classArray: [AnyClass], searchOptions searchOptions: [String : AnyObject] = [:], using block: (NSDraggingItem, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(OSX 10.11, *)
  var springLoadingHighlight: NSSpringLoadingHighlight { get }
  @available(OSX 10.11, *)
  func resetSpringLoading()
}
protocol NSDraggingDestination : NSObjectProtocol {
  @discardableResult
  optional func draggingEntered(_ sender: NSDraggingInfo) -> NSDragOperation
  @discardableResult
  optional func draggingUpdated(_ sender: NSDraggingInfo) -> NSDragOperation
  optional func draggingExited(_ sender: NSDraggingInfo?)
  @discardableResult
  optional func prepare(forDragOperation sender: NSDraggingInfo) -> Bool
  @discardableResult
  optional func performDragOperation(_ sender: NSDraggingInfo) -> Bool
  optional func concludeDragOperation(_ sender: NSDraggingInfo?)
  optional func draggingEnded(_ sender: NSDraggingInfo?)
  @discardableResult
  optional func wantsPeriodicDraggingUpdates() -> Bool
  @available(OSX 10.7, *)
  optional func updateDraggingItems(forDrag sender: NSDraggingInfo?)
}
protocol NSDraggingSource : NSObjectProtocol {
  @available(OSX 10.7, *)
  @discardableResult
  func draggingSession(_ session: NSDraggingSession, sourceOperationMaskFor context: NSDraggingContext) -> NSDragOperation
  @available(OSX 10.7, *)
  optional func draggingSession(_ session: NSDraggingSession, willBeginAt screenPoint: NSPoint)
  @available(OSX 10.7, *)
  optional func draggingSession(_ session: NSDraggingSession, movedTo screenPoint: NSPoint)
  @available(OSX 10.7, *)
  optional func draggingSession(_ session: NSDraggingSession, endedAt screenPoint: NSPoint, operation operation: NSDragOperation)
  @available(OSX 10.7, *)
  @discardableResult
  optional func ignoreModifierKeys(for session: NSDraggingSession) -> Bool
}
@available(OSX 10.11, *)
struct NSSpringLoadingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var enabled: NSSpringLoadingOptions { get }
  static var continuousActivation: NSSpringLoadingOptions { get }
  static var noHover: NSSpringLoadingOptions { get }
}
protocol NSSpringLoadingDestination : NSObjectProtocol {
  @available(OSX 10.11, *)
  func springLoadingActivated(_ activated: Bool, draggingInfo draggingInfo: NSDraggingInfo)
  @available(OSX 10.11, *)
  func springLoadingHighlightChanged(_ draggingInfo: NSDraggingInfo)
  @available(OSX 10.11, *)
  @discardableResult
  optional func springLoadingEntered(_ draggingInfo: NSDraggingInfo) -> NSSpringLoadingOptions
  @available(OSX 10.11, *)
  @discardableResult
  optional func springLoadingUpdated(_ draggingInfo: NSDraggingInfo) -> NSSpringLoadingOptions
  @available(OSX 10.11, *)
  optional func springLoadingExited(_ draggingInfo: NSDraggingInfo)
  @available(OSX 10.11, *)
  optional func draggingEnded(_ draggingInfo: NSDraggingInfo)
}
extension NSObject {
  @discardableResult
  class func namesOfPromisedFilesDropped(atDestination dropDestination: NSURL) -> [String]?
  @discardableResult
  func namesOfPromisedFilesDropped(atDestination dropDestination: NSURL) -> [String]?
}
