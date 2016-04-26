
var NSSearchFieldRecentsTitleMenuItemTag: Int32 { get }
var NSSearchFieldRecentsMenuItemTag: Int32 { get }
var NSSearchFieldClearRecentsMenuItemTag: Int32 { get }
var NSSearchFieldNoRecentsMenuItemTag: Int32 { get }
class NSSearchFieldCell : NSTextFieldCell {
  var searchButtonCell: NSButtonCell?
  var cancelButtonCell: NSButtonCell?
  func resetSearchButtonCell()
  func resetCancelButtonCell()
  @discardableResult
  func searchTextRect(forBounds rect: NSRect) -> NSRect
  @discardableResult
  func searchButtonRect(forBounds rect: NSRect) -> NSRect
  @discardableResult
  func cancelButtonRect(forBounds rect: NSRect) -> NSRect
  var searchMenuTemplate: NSMenu?
  var sendsWholeSearchString: Bool
  var maximumRecents: Int
  var recentSearches: [String]!
  var recentsAutosaveName: String?
  var sendsSearchStringImmediately: Bool
}
struct __sfFlags {
  var sendsWholeSearchString: UInt32
  var maximumRecents: UInt32
  var cancelVisible: UInt32
  var reserved2: UInt32
  var disableText: UInt32
  var menuTracking: UInt32
  var deferredUpdate: UInt32
  var sendsImmediately: UInt32
  var centeredLook: UInt32
  var renderingCentered: UInt32
  var becomeTransition: UInt32
  var resignTransition: UInt32
  var subclassOverridesRectForSearchButtonWhenCentered: UInt32
  var subclassOverridesRectForSearchTextWhenCentered: UInt32
  var subclassOverridesRectForCancelButtonWhenCentered: UInt32
  var resumeEditingOnCancel: UInt32
  var reserved: UInt32
  init()
  init(sendsWholeSearchString sendsWholeSearchString: UInt32, maximumRecents maximumRecents: UInt32, cancelVisible cancelVisible: UInt32, reserved2 reserved2: UInt32, disableText disableText: UInt32, menuTracking menuTracking: UInt32, deferredUpdate deferredUpdate: UInt32, sendsImmediately sendsImmediately: UInt32, centeredLook centeredLook: UInt32, renderingCentered renderingCentered: UInt32, becomeTransition becomeTransition: UInt32, resignTransition resignTransition: UInt32, subclassOverridesRectForSearchButtonWhenCentered subclassOverridesRectForSearchButtonWhenCentered: UInt32, subclassOverridesRectForSearchTextWhenCentered subclassOverridesRectForSearchTextWhenCentered: UInt32, subclassOverridesRectForCancelButtonWhenCentered subclassOverridesRectForCancelButtonWhenCentered: UInt32, resumeEditingOnCancel resumeEditingOnCancel: UInt32, reserved reserved: UInt32)
}
