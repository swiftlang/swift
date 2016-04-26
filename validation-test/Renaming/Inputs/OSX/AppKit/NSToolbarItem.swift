
class NSToolbarItem : NSObject, NSCopying, NSValidatedUserInterfaceItem {
  init(itemIdentifier itemIdentifier: String)
  var itemIdentifier: String { get }
  unowned(unsafe) var toolbar: @sil_unmanaged NSToolbar? { get }
  var label: String
  var paletteLabel: String
  var toolTip: String?
  var menuFormRepresentation: NSMenuItem?
  weak var target: @sil_weak AnyObject?
  var isEnabled: Bool
  var image: NSImage?
  var view: NSView?
  var minSize: NSSize
  var maxSize: NSSize
  var visibilityPriority: Int
  func validate()
  var autovalidates: Bool
  var allowsDuplicatesInToolbar: Bool { get }
}
struct __tbiFlags {
  var viewRespondsToIsEnabled: UInt32
  var viewRespondsToSetEnabled: UInt32
  var viewRespondsToTag: UInt32
  var viewRespondsToSetTag: UInt32
  var viewRespondsToAction: UInt32
  var viewRespondsToSetAction: UInt32
  var viewRespondsToTarget: UInt32
  var viewRespondsToSetTarget: UInt32
  var viewRespondsToImage: UInt32
  var viewRespondsToSetImage: UInt32
  var isEnabled: UInt32
  var isUserRemovable: UInt32
  var menuHasBeenSet: UInt32
  var menuRepIsDefault: UInt32
  var viewHasBeenLoaded: UInt32
  var drawingForDragImage: UInt32
  var isCustomItemType: UInt32
  var hasValidatedAutoModeConfiguration: UInt32
  var useAutoModeConfiguration: UInt32
  var fromBaseLocalizedNib: UInt32
  var autovalidationDisabled: UInt32
  var tagHasBeenSet: UInt32
  var sizeHasBeenSet: UInt32
  var stateWasDisabledBeforeSheet: UInt32
  var wantsToBeCentered: UInt32
  var RESERVED: UInt32
  init()
  init(viewRespondsToIsEnabled viewRespondsToIsEnabled: UInt32, viewRespondsToSetEnabled viewRespondsToSetEnabled: UInt32, viewRespondsToTag viewRespondsToTag: UInt32, viewRespondsToSetTag viewRespondsToSetTag: UInt32, viewRespondsToAction viewRespondsToAction: UInt32, viewRespondsToSetAction viewRespondsToSetAction: UInt32, viewRespondsToTarget viewRespondsToTarget: UInt32, viewRespondsToSetTarget viewRespondsToSetTarget: UInt32, viewRespondsToImage viewRespondsToImage: UInt32, viewRespondsToSetImage viewRespondsToSetImage: UInt32, isEnabled isEnabled: UInt32, isUserRemovable isUserRemovable: UInt32, menuHasBeenSet menuHasBeenSet: UInt32, menuRepIsDefault menuRepIsDefault: UInt32, viewHasBeenLoaded viewHasBeenLoaded: UInt32, drawingForDragImage drawingForDragImage: UInt32, isCustomItemType isCustomItemType: UInt32, hasValidatedAutoModeConfiguration hasValidatedAutoModeConfiguration: UInt32, useAutoModeConfiguration useAutoModeConfiguration: UInt32, fromBaseLocalizedNib fromBaseLocalizedNib: UInt32, autovalidationDisabled autovalidationDisabled: UInt32, tagHasBeenSet tagHasBeenSet: UInt32, sizeHasBeenSet sizeHasBeenSet: UInt32, stateWasDisabledBeforeSheet stateWasDisabledBeforeSheet: UInt32, wantsToBeCentered wantsToBeCentered: UInt32, RESERVED RESERVED: UInt32)
}
var NSToolbarItemVisibilityPriorityStandard: Int { get }
var NSToolbarItemVisibilityPriorityLow: Int { get }
var NSToolbarItemVisibilityPriorityHigh: Int { get }
var NSToolbarItemVisibilityPriorityUser: Int { get }
extension NSObject {
  @discardableResult
  class func validate(_ theItem: NSToolbarItem) -> Bool
  @discardableResult
  func validate(_ theItem: NSToolbarItem) -> Bool
}
let NSToolbarSeparatorItemIdentifier: String
let NSToolbarSpaceItemIdentifier: String
let NSToolbarFlexibleSpaceItemIdentifier: String
let NSToolbarShowColorsItemIdentifier: String
let NSToolbarShowFontsItemIdentifier: String
let NSToolbarCustomizeToolbarItemIdentifier: String
let NSToolbarPrintItemIdentifier: String
@available(OSX 10.11, *)
let NSToolbarToggleSidebarItemIdentifier: String
