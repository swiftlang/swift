
enum NSToolbarDisplayMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case iconAndLabel
  case iconOnly
  case labelOnly
}
enum NSToolbarSizeMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case regular
  case small
}
class NSToolbar : NSObject {
  init(identifier identifier: String)
  func insertItem(withItemIdentifier itemIdentifier: String, at index: Int)
  func removeItem(at index: Int)
  unowned(unsafe) var delegate: @sil_unmanaged NSToolbarDelegate?
  var isVisible: Bool
  func runCustomizationPalette(_ sender: AnyObject?)
  var customizationPaletteIsRunning: Bool { get }
  var displayMode: NSToolbarDisplayMode
  var selectedItemIdentifier: String?
  var sizeMode: NSToolbarSizeMode
  var showsBaselineSeparator: Bool
  var allowsUserCustomization: Bool
  var identifier: String { get }
  var items: [NSToolbarItem] { get }
  var visibleItems: [NSToolbarItem]? { get }
  var autosavesConfiguration: Bool
  func setConfigurationFrom(_ configDict: [String : AnyObject])
  var configurationDictionary: [String : AnyObject] { get }
  func validateVisibleItems()
  @available(OSX 10.10, *)
  var allowsExtensionItems: Bool
}
struct __tbFlags {
  var allowsUserCustomization: UInt32
  var autosavesUsingIdentifier: UInt32
  var initialConfigurationDone: UInt32
  var doesNotAttachToMenuBar: UInt32
  var delegateDefaultItemIdentifiers: UInt32
  var delegateAllowedItemIdentifiers: UInt32
  var delegateItemWithItemIdentifier: UInt32
  var delegateNotificationsEnabled: UInt32
  var prefersToBeShown: UInt32
  var loadItemsImmediately: UInt32
  var currentItemsContainsPlaceholder: UInt32
  var customizationPanelIsRunning: UInt32
  var usesCustomSheetWidth: UInt32
  var clickAndDragPerformsCustomization: UInt32
  var showsNoContextMenu: UInt32
  var currentlyLoadingPlaceholders: UInt32
  var delegateItemWithItemIdentifier2: UInt32
  var inGlobalWindow: UInt32
  var hasOwnedFullscreenViewController: UInt32
  var usesServicesItems: UInt32
  var usingFSMetrics: UInt32
  var keyboardLoopNeedsUpdating: UInt32
  var showHideDuringConfigurationChangeDisabled: UInt32
  var displayMode: UInt32
  var sizeMode: UInt32
  var doNotShowBaselineSeparator: UInt32
  var hideWithoutResizingWindowHint: UInt32
  var autovalidatesItemsDisabled: UInt32
  var inAutovalidation: UInt32
  var loadedMetrics: UInt32
  init()
  init(allowsUserCustomization allowsUserCustomization: UInt32, autosavesUsingIdentifier autosavesUsingIdentifier: UInt32, initialConfigurationDone initialConfigurationDone: UInt32, doesNotAttachToMenuBar doesNotAttachToMenuBar: UInt32, delegateDefaultItemIdentifiers delegateDefaultItemIdentifiers: UInt32, delegateAllowedItemIdentifiers delegateAllowedItemIdentifiers: UInt32, delegateItemWithItemIdentifier delegateItemWithItemIdentifier: UInt32, delegateNotificationsEnabled delegateNotificationsEnabled: UInt32, prefersToBeShown prefersToBeShown: UInt32, loadItemsImmediately loadItemsImmediately: UInt32, currentItemsContainsPlaceholder currentItemsContainsPlaceholder: UInt32, customizationPanelIsRunning customizationPanelIsRunning: UInt32, usesCustomSheetWidth usesCustomSheetWidth: UInt32, clickAndDragPerformsCustomization clickAndDragPerformsCustomization: UInt32, showsNoContextMenu showsNoContextMenu: UInt32, currentlyLoadingPlaceholders currentlyLoadingPlaceholders: UInt32, delegateItemWithItemIdentifier2 delegateItemWithItemIdentifier2: UInt32, inGlobalWindow inGlobalWindow: UInt32, hasOwnedFullscreenViewController hasOwnedFullscreenViewController: UInt32, usesServicesItems usesServicesItems: UInt32, usingFSMetrics usingFSMetrics: UInt32, keyboardLoopNeedsUpdating keyboardLoopNeedsUpdating: UInt32, showHideDuringConfigurationChangeDisabled showHideDuringConfigurationChangeDisabled: UInt32, displayMode displayMode: UInt32, sizeMode sizeMode: UInt32, doNotShowBaselineSeparator doNotShowBaselineSeparator: UInt32, hideWithoutResizingWindowHint hideWithoutResizingWindowHint: UInt32, autovalidatesItemsDisabled autovalidatesItemsDisabled: UInt32, inAutovalidation inAutovalidation: UInt32, loadedMetrics loadedMetrics: UInt32)
}
protocol NSToolbarDelegate : NSObjectProtocol {
  @discardableResult
  optional func toolbar(_ toolbar: NSToolbar, itemForItemIdentifier itemIdentifier: String, willBeInsertedIntoToolbar flag: Bool) -> NSToolbarItem?
  @discardableResult
  optional func toolbarDefaultItemIdentifiers(_ toolbar: NSToolbar) -> [String]
  @discardableResult
  optional func toolbarAllowedItemIdentifiers(_ toolbar: NSToolbar) -> [String]
  @discardableResult
  optional func toolbarSelectableItemIdentifiers(_ toolbar: NSToolbar) -> [String]
  optional func toolbarWillAddItem(_ notification: NSNotification)
  optional func toolbarDidRemoveItem(_ notification: NSNotification)
}
let NSToolbarWillAddItemNotification: String
let NSToolbarDidRemoveItemNotification: String
extension NSToolbar {
  @available(OSX 10.7, *)
  var fullScreenAccessoryView: NSView?
  @available(OSX 10.7, *)
  var fullScreenAccessoryViewMinHeight: CGFloat
  @available(OSX 10.7, *)
  var fullScreenAccessoryViewMaxHeight: CGFloat
}
