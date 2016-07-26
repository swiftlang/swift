
enum NSCellType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case nullCellType
  case textCellType
  case imageCellType
}
enum NSCellAttribute : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case cellDisabled
  case cellState
  case pushInCell
  case cellEditable
  case changeGrayCell
  case cellHighlighted
  case cellLightsByContents
  case cellLightsByGray
  case changeBackgroundCell
  case cellLightsByBackground
  case cellIsBordered
  case cellHasOverlappingImage
  case cellHasImageHorizontal
  case cellHasImageOnLeftOrBottom
  case cellChangesContents
  case cellIsInsetButton
  case cellAllowsMixedState
}
enum NSCellImagePosition : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case noImage
  case imageOnly
  case imageLeft
  case imageRight
  case imageBelow
  case imageAbove
  case imageOverlaps
}
@available(OSX 10.5, *)
enum NSImageScaling : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case scaleProportionallyDown
  case scaleAxesIndependently
  case scaleNone
  case scaleProportionallyUpOrDown
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use NSImageScaleProportionallyDown instead")
  static var NSScaleProportionally: NSImageScaling { get }
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use NSImageScaleAxesIndependently instead")
  static var NSScaleToFit: NSImageScaling { get }
  @available(OSX, introduced: 10.0, deprecated: 10.10, message: "Use NSImageScaleNone instead")
  static var NSScaleNone: NSImageScaling { get }
}
var NSMixedState: Int { get }
var NSOffState: Int { get }
var NSOnState: Int { get }
typealias NSCellStateValue = Int
struct NSCellStyleMask : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var contentsCellMask: NSCellStyleMask { get }
  static var pushInCellMask: NSCellStyleMask { get }
  static var changeGrayCellMask: NSCellStyleMask { get }
  static var changeBackgroundCellMask: NSCellStyleMask { get }
}
enum NSControlTint : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case defaultControlTint
  case blueControlTint
  case graphiteControlTint
  case clearControlTint
}
enum NSControlSize : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case regularControlSize
  case smallControlSize
  case miniControlSize
}
struct __CFlags {
  var state: UInt32
  var highlighted: UInt32
  var disabled: UInt32
  var editable: UInt32
  var type: NSCellType
  var vCentered: UInt32
  var hCentered: UInt32
  var bordered: UInt32
  var bezeled: UInt32
  var selectable: UInt32
  var scrollable: UInt32
  var continuous: UInt32
  var actOnMouseDown: UInt32
  var isLeaf: UInt32
  var invalidObjectValue: UInt32
  var invalidFont: UInt32
  var lineBreakMode: NSLineBreakMode
  var weakTargetHelperFlag: UInt32
  var allowsAppearanceEffects: UInt32
  var singleLineMode: UInt32
  var actOnMouseDragged: UInt32
  var isLoaded: UInt32
  var truncateLastLine: UInt32
  var dontActOnMouseUp: UInt32
  var isWhite: UInt32
  var useUserKeyEquivalent: UInt32
  var showsFirstResponder: UInt32
  var focusRingType: UInt32
  var wasSelectable: UInt32
  var hasInvalidObject: UInt32
  var allowsEditingTextAttributes: UInt32
  var importsGraphics: UInt32
  var alignment: NSTextAlignment
  var layoutDirectionRTL: UInt32
  var backgroundStyle: UInt32
  var cellReserved2: UInt32
  var refusesFirstResponder: UInt32
  var needsHighlightedText: UInt32
  var dontAllowsUndo: UInt32
  var currentlyEditing: UInt32
  var allowsMixedState: UInt32
  var inMixedState: UInt32
  var sendsActionOnEndEditing: UInt32
  var inSendAction: UInt32
  var menuWasSet: UInt32
  var controlTint: UInt32
  var controlSize: UInt32
  var branchImageDisabled: UInt32
  var drawingInRevealover: UInt32
  var needsHighlightedTextHint: UInt32
  init()
  init(state state: UInt32, highlighted highlighted: UInt32, disabled disabled: UInt32, editable editable: UInt32, type type: NSCellType, vCentered vCentered: UInt32, hCentered hCentered: UInt32, bordered bordered: UInt32, bezeled bezeled: UInt32, selectable selectable: UInt32, scrollable scrollable: UInt32, continuous continuous: UInt32, actOnMouseDown actOnMouseDown: UInt32, isLeaf isLeaf: UInt32, invalidObjectValue invalidObjectValue: UInt32, invalidFont invalidFont: UInt32, lineBreakMode lineBreakMode: NSLineBreakMode, weakTargetHelperFlag weakTargetHelperFlag: UInt32, allowsAppearanceEffects allowsAppearanceEffects: UInt32, singleLineMode singleLineMode: UInt32, actOnMouseDragged actOnMouseDragged: UInt32, isLoaded isLoaded: UInt32, truncateLastLine truncateLastLine: UInt32, dontActOnMouseUp dontActOnMouseUp: UInt32, isWhite isWhite: UInt32, useUserKeyEquivalent useUserKeyEquivalent: UInt32, showsFirstResponder showsFirstResponder: UInt32, focusRingType focusRingType: UInt32, wasSelectable wasSelectable: UInt32, hasInvalidObject hasInvalidObject: UInt32, allowsEditingTextAttributes allowsEditingTextAttributes: UInt32, importsGraphics importsGraphics: UInt32, alignment alignment: NSTextAlignment, layoutDirectionRTL layoutDirectionRTL: UInt32, backgroundStyle backgroundStyle: UInt32, cellReserved2 cellReserved2: UInt32, refusesFirstResponder refusesFirstResponder: UInt32, needsHighlightedText needsHighlightedText: UInt32, dontAllowsUndo dontAllowsUndo: UInt32, currentlyEditing currentlyEditing: UInt32, allowsMixedState allowsMixedState: UInt32, inMixedState inMixedState: UInt32, sendsActionOnEndEditing sendsActionOnEndEditing: UInt32, inSendAction inSendAction: UInt32, menuWasSet menuWasSet: UInt32, controlTint controlTint: UInt32, controlSize controlSize: UInt32, branchImageDisabled branchImageDisabled: UInt32, drawingInRevealover drawingInRevealover: UInt32, needsHighlightedTextHint needsHighlightedTextHint: UInt32)
}
typealias _CFlags = __CFlags
class NSCell : NSObject, NSCopying, NSCoding, NSUserInterfaceItemIdentification, NSAccessibilityElementProtocol, NSAccessibility {
  @discardableResult
  class func prefersTrackingUntilMouseUp() -> Bool
  init(textCell aString: String)
  init(imageCell image: NSImage?)
  unowned(unsafe) var controlView: @sil_unmanaged NSView?
  var type: NSCellType
  var state: Int
  weak var target: @sil_weak AnyObject?
  var action: Selector?
  var tag: Int
  var title: String
  var isOpaque: Bool { get }
  var isEnabled: Bool
  @discardableResult
  func sendAction(on mask: Int) -> Int
  var isContinuous: Bool
  var isEditable: Bool
  var isSelectable: Bool
  var isBordered: Bool
  var isBezeled: Bool
  var isScrollable: Bool
  var isHighlighted: Bool
  var alignment: NSTextAlignment
  var wraps: Bool
  var font: NSFont?
  var keyEquivalent: String { get }
  var formatter: NSFormatter?
  @NSCopying var objectValue: AnyObject?
  var hasValidObjectValue: Bool { get }
  var stringValue: String
  @discardableResult
  func compare(_ otherCell: AnyObject) -> NSComparisonResult
  var intValue: Int32
  var floatValue: Float
  var doubleValue: Double
  func takeIntValueFrom(_ sender: AnyObject?)
  func takeFloatValueFrom(_ sender: AnyObject?)
  func takeDoubleValueFrom(_ sender: AnyObject?)
  func takeStringValueFrom(_ sender: AnyObject?)
  func takeObjectValueFrom(_ sender: AnyObject?)
  var image: NSImage?
  var controlTint: NSControlTint
  var controlSize: NSControlSize
  var representedObject: AnyObject?
  @discardableResult
  func cellAttribute(_ aParameter: NSCellAttribute) -> Int
  func setCellAttribute(_ aParameter: NSCellAttribute, to value: Int)
  @discardableResult
  func imageRect(forBounds theRect: NSRect) -> NSRect
  @discardableResult
  func titleRect(forBounds theRect: NSRect) -> NSRect
  @discardableResult
  func drawingRect(forBounds theRect: NSRect) -> NSRect
  var cellSize: NSSize { get }
  @discardableResult
  func cellSize(forBounds aRect: NSRect) -> NSSize
  @discardableResult
  func highlightColor(withFrame cellFrame: NSRect, in controlView: NSView) -> NSColor
  func calcDrawInfo(_ aRect: NSRect)
  @discardableResult
  func setUpFieldEditorAttributes(_ textObj: NSText) -> NSText
  func drawInterior(withFrame cellFrame: NSRect, in controlView: NSView)
  func draw(withFrame cellFrame: NSRect, in controlView: NSView)
  func highlight(_ flag: Bool, withFrame cellFrame: NSRect, in controlView: NSView)
  var mouseDownFlags: Int { get }
  func getPeriodicDelay(_ delay: UnsafeMutablePointer<Float>, interval interval: UnsafeMutablePointer<Float>)
  @discardableResult
  func startTracking(at startPoint: NSPoint, in controlView: NSView) -> Bool
  @discardableResult
  func continueTracking(_ lastPoint: NSPoint, at currentPoint: NSPoint, in controlView: NSView) -> Bool
  func stopTracking(_ lastPoint: NSPoint, at stopPoint: NSPoint, in controlView: NSView, mouseIsUp flag: Bool)
  @discardableResult
  func trackMouse(_ theEvent: NSEvent, in cellFrame: NSRect, of controlView: NSView, untilMouseUp flag: Bool) -> Bool
  func edit(withFrame aRect: NSRect, in controlView: NSView, editor textObj: NSText, delegate anObject: AnyObject?, event theEvent: NSEvent)
  func select(withFrame aRect: NSRect, in controlView: NSView, editor textObj: NSText, delegate anObject: AnyObject?, start selStart: Int, length selLength: Int)
  func endEditing(_ textObj: NSText)
  func resetCursorRect(_ cellFrame: NSRect, in controlView: NSView)
  var menu: NSMenu?
  @discardableResult
  func menu(for event: NSEvent, in cellFrame: NSRect, of view: NSView) -> NSMenu?
  @discardableResult
  class func defaultMenu() -> NSMenu?
  var sendsActionOnEndEditing: Bool
  var baseWritingDirection: NSWritingDirection
  var lineBreakMode: NSLineBreakMode
  var allowsUndo: Bool
  @available(OSX 10.5, *)
  var integerValue: Int
  @available(OSX 10.5, *)
  func takeIntegerValueFrom(_ sender: AnyObject?)
  @available(OSX 10.5, *)
  var truncatesLastVisibleLine: Bool
  @available(OSX 10.6, *)
  var userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection
  @available(OSX 10.6, *)
  @discardableResult
  func fieldEditor(for aControlView: NSView) -> NSTextView?
  @available(OSX 10.6, *)
  var usesSingleLineMode: Bool
  @available(OSX 10.7, *)
  @discardableResult
  func draggingImageComponents(withFrame frame: NSRect, in view: NSView) -> [NSDraggingImageComponent]
}
extension NSCell {
  var refusesFirstResponder: Bool
  var acceptsFirstResponder: Bool { get }
  var showsFirstResponder: Bool
  func performClick(_ sender: AnyObject?)
  var focusRingType: NSFocusRingType
  @discardableResult
  class func defaultFocusRingType() -> NSFocusRingType
  @available(OSX 10.7, *)
  func drawFocusRingMask(withFrame cellFrame: NSRect, in controlView: NSView)
  @available(OSX 10.7, *)
  @discardableResult
  func focusRingMaskBounds(forFrame cellFrame: NSRect, in controlView: NSView) -> NSRect
  var wantsNotificationForMarkedText: Bool { get }
}
extension NSCell {
  @NSCopying var attributedStringValue: NSAttributedString
  var allowsEditingTextAttributes: Bool
  var importsGraphics: Bool
}
extension NSCell {
  var allowsMixedState: Bool
  var nextState: Int { get }
  func setNextState()
}
let NSControlTintDidChangeNotification: String
@available(OSX 10.5, *)
struct NSCellHitResult : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var contentArea: NSCellHitResult { get }
  static var editableTextArea: NSCellHitResult { get }
  static var trackableArea: NSCellHitResult { get }
}
extension NSCell {
  @available(OSX 10.5, *)
  @discardableResult
  func hitTest(for event: NSEvent, in cellFrame: NSRect, of controlView: NSView) -> NSCellHitResult
}
extension NSCell {
  @available(OSX 10.5, *)
  @discardableResult
  func expansionFrame(withFrame cellFrame: NSRect, in view: NSView) -> NSRect
  @available(OSX 10.5, *)
  func draw(withExpansionFrame cellFrame: NSRect, in view: NSView)
}
@available(OSX 10.5, *)
enum NSBackgroundStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case light
  case dark
  case raised
  case lowered
}
extension NSCell {
  @available(OSX 10.5, *)
  var backgroundStyle: NSBackgroundStyle
  @available(OSX 10.5, *)
  var interiorBackgroundStyle: NSBackgroundStyle { get }
}
extension NSCell {
}
@available(OSX 10.5, *)
func NSDrawThreePartImage(_ frame: NSRect, _ startCap: NSImage?, _ centerFill: NSImage?, _ endCap: NSImage?, _ vertical: Bool, _ op: NSCompositingOperation, _ alphaFraction: CGFloat, _ flipped: Bool)
@available(OSX 10.5, *)
func NSDrawNinePartImage(_ frame: NSRect, _ topLeftCorner: NSImage, _ topEdgeFill: NSImage, _ topRightCorner: NSImage, _ leftEdgeFill: NSImage, _ centerFill: NSImage, _ rightEdgeFill: NSImage, _ bottomLeftCorner: NSImage, _ bottomEdgeFill: NSImage, _ bottomRightCorner: NSImage, _ op: NSCompositingOperation, _ alphaFraction: CGFloat, _ flipped: Bool)
var NSAnyType: Int { get }
var NSIntType: Int { get }
var NSPositiveIntType: Int { get }
var NSFloatType: Int { get }
var NSPositiveFloatType: Int { get }
var NSDoubleType: Int { get }
var NSPositiveDoubleType: Int { get }
