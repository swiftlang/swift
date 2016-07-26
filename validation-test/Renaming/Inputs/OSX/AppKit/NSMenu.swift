
class NSMenu : NSObject, NSCopying, NSCoding {
  init(title aTitle: String)
  var title: String
  class func popUpContextMenu(_ menu: NSMenu, with event: NSEvent, for view: NSView)
  class func popUpContextMenu(_ menu: NSMenu, with event: NSEvent, for view: NSView, with font: NSFont?)
  @available(OSX 10.6, *)
  @discardableResult
  func popUpPositioningItem(_ item: NSMenuItem?, atLocation location: NSPoint, in view: NSView?) -> Bool
  class func setMenuBarVisible(_ visible: Bool)
  @discardableResult
  class func menuBarVisible() -> Bool
  unowned(unsafe) var supermenu: @sil_unmanaged NSMenu?
  func insert(_ newItem: NSMenuItem, at index: Int)
  func add(_ newItem: NSMenuItem)
  @discardableResult
  func insertItem(withTitle aString: String, action aSelector: Selector?, keyEquivalent charCode: String, at index: Int) -> NSMenuItem?
  @discardableResult
  func addItem(withTitle aString: String, action aSelector: Selector?, keyEquivalent charCode: String) -> NSMenuItem?
  func removeItem(at index: Int)
  func remove(_ item: NSMenuItem)
  func setSubmenu(_ aMenu: NSMenu?, for anItem: NSMenuItem)
  @available(OSX 10.6, *)
  func removeAllItems()
  var itemArray: [NSMenuItem] { get }
  var numberOfItems: Int { get }
  @discardableResult
  func item(at index: Int) -> NSMenuItem?
  @discardableResult
  func index(of item: NSMenuItem) -> Int
  @discardableResult
  func indexOfItem(withTitle aTitle: String) -> Int
  @discardableResult
  func indexOfItem(withTag aTag: Int) -> Int
  @discardableResult
  func indexOfItem(withRepresentedObject object: AnyObject) -> Int
  @discardableResult
  func indexOfItem(withSubmenu submenu: NSMenu?) -> Int
  @discardableResult
  func indexOfItem(withTarget target: AnyObject?, andAction actionSelector: Selector) -> Int
  @discardableResult
  func item(withTitle aTitle: String) -> NSMenuItem?
  @discardableResult
  func item(withTag tag: Int) -> NSMenuItem?
  var autoenablesItems: Bool
  func update()
  @discardableResult
  func performKeyEquivalent(_ theEvent: NSEvent) -> Bool
  func itemChanged(_ item: NSMenuItem)
  func performActionForItem(at index: Int)
  unowned(unsafe) var delegate: @sil_unmanaged NSMenuDelegate?
  var menuBarHeight: CGFloat { get }
  @available(OSX 10.5, *)
  func cancelTracking()
  @available(OSX 10.6, *)
  func cancelTrackingWithoutAnimation()
  @available(OSX 10.5, *)
  var highlightedItem: NSMenuItem? { get }
  @available(OSX 10.6, *)
  var minimumWidth: CGFloat
  @available(OSX 10.6, *)
  var size: NSSize { get }
  @available(OSX 10.6, *)
  var font: NSFont!
  @available(OSX 10.6, *)
  var allowsContextMenuPlugIns: Bool
  @available(OSX 10.5, *)
  var showsStateColumn: Bool
  @available(OSX 10.11, *)
  var userInterfaceLayoutDirection: NSUserInterfaceLayoutDirection
}
struct __mFlags {
  var noAutoenable: UInt32
  var inMain: UInt32
  var internalPerformAction: UInt32
  var condenseSeparators: UInt32
  var disabled: UInt32
  var ownedByPopUp: UInt32
  var delegateNeedsUpdate: UInt32
  var delegateUpdateItem: UInt32
  var delegateHasKeyEquiv: UInt32
  var delegateHasAltKeyEquiv: UInt32
  var excludeMarkColumn: UInt32
  var isContextualMenu: UInt32
  var cmPluginMode: UInt32
  var invertedCMPluginTypes: UInt32
  var allowsDifferentSelection: UInt32
  var noTopPadding: UInt32
  var noBottomPadding: UInt32
  var hasNCStyle: UInt32
  var delegateIsUnsafeUnretained: UInt32
  var RESERVED: UInt32
  init()
  init(noAutoenable noAutoenable: UInt32, inMain inMain: UInt32, internalPerformAction internalPerformAction: UInt32, condenseSeparators condenseSeparators: UInt32, disabled disabled: UInt32, ownedByPopUp ownedByPopUp: UInt32, delegateNeedsUpdate delegateNeedsUpdate: UInt32, delegateUpdateItem delegateUpdateItem: UInt32, delegateHasKeyEquiv delegateHasKeyEquiv: UInt32, delegateHasAltKeyEquiv delegateHasAltKeyEquiv: UInt32, excludeMarkColumn excludeMarkColumn: UInt32, isContextualMenu isContextualMenu: UInt32, cmPluginMode cmPluginMode: UInt32, invertedCMPluginTypes invertedCMPluginTypes: UInt32, allowsDifferentSelection allowsDifferentSelection: UInt32, noTopPadding noTopPadding: UInt32, noBottomPadding noBottomPadding: UInt32, hasNCStyle hasNCStyle: UInt32, delegateIsUnsafeUnretained delegateIsUnsafeUnretained: UInt32, RESERVED RESERVED: UInt32)
}
extension NSMenu {
  func submenuAction(_ sender: AnyObject?)
}
extension NSObject {
  @discardableResult
  class func validate(_ menuItem: NSMenuItem) -> Bool
  @discardableResult
  func validate(_ menuItem: NSMenuItem) -> Bool
}
protocol NSMenuDelegate : NSObjectProtocol {
  optional func menuNeedsUpdate(_ menu: NSMenu)
  @discardableResult
  optional func numberOfItems(in menu: NSMenu) -> Int
  @discardableResult
  optional func menu(_ menu: NSMenu, update item: NSMenuItem, at index: Int, shouldCancel shouldCancel: Bool) -> Bool
  @discardableResult
  optional func menuHasKeyEquivalent(_ menu: NSMenu, for event: NSEvent, target target: AutoreleasingUnsafeMutablePointer<AnyObject?>?, action action: UnsafeMutablePointer<Selector?>?) -> Bool
  @available(OSX 10.5, *)
  optional func menuWillOpen(_ menu: NSMenu)
  @available(OSX 10.5, *)
  optional func menuDidClose(_ menu: NSMenu)
  @available(OSX 10.5, *)
  optional func menu(_ menu: NSMenu, willHighlight item: NSMenuItem?)
  @available(OSX 10.6, *)
  @discardableResult
  optional func confinementRect(for menu: NSMenu, on screen: NSScreen?) -> NSRect
}
struct NSMenuProperties : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var propertyItemTitle: NSMenuProperties { get }
  static var propertyItemAttributedTitle: NSMenuProperties { get }
  static var propertyItemKeyEquivalent: NSMenuProperties { get }
  static var propertyItemImage: NSMenuProperties { get }
  static var propertyItemEnabled: NSMenuProperties { get }
  static var propertyItemAccessibilityDescription: NSMenuProperties { get }
}
extension NSMenu {
  @available(OSX 10.6, *)
  var propertiesToUpdate: NSMenuProperties { get }
}
let NSMenuWillSendActionNotification: String
let NSMenuDidSendActionNotification: String
let NSMenuDidAddItemNotification: String
let NSMenuDidRemoveItemNotification: String
let NSMenuDidChangeItemNotification: String
let NSMenuDidBeginTrackingNotification: String
let NSMenuDidEndTrackingNotification: String
extension NSMenu {
  @available(OSX, introduced: 10.0, deprecated: 10.11)
  @discardableResult
  class func menuZone() -> NSZone!
  @available(OSX, introduced: 10.0, deprecated: 10.11)
  var menuChangedMessagesEnabled: Bool
  @available(OSX, introduced: 10.0, deprecated: 10.11)
  func helpRequested(_ eventPtr: NSEvent)
  @available(OSX, introduced: 10.0, deprecated: 10.11)
  var isTornOff: Bool { get }
}
