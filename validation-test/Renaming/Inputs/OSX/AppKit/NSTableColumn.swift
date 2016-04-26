
struct NSTableColumnResizingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var autoresizingMask: NSTableColumnResizingOptions { get }
  static var userResizingMask: NSTableColumnResizingOptions { get }
}
class NSTableColumn : NSObject, NSCoding, NSUserInterfaceItemIdentification {
  init(identifier identifier: String)
  unowned(unsafe) var tableView: @sil_unmanaged NSTableView?
  var width: CGFloat
  var minWidth: CGFloat
  var maxWidth: CGFloat
  var title: String
  var headerCell: NSTableHeaderCell
  var isEditable: Bool
  func sizeToFit()
  @NSCopying var sortDescriptorPrototype: NSSortDescriptor?
  var resizingMask: NSTableColumnResizingOptions
  @available(OSX 10.5, *)
  var headerToolTip: String?
  @available(OSX 10.5, *)
  var isHidden: Bool
}
struct __colFlags {
  var oldIsResizable: UInt32
  var isEditable: UInt32
  var resizedPostingDisableCount: UInt32
  var canUseReorderResizeImageCache: UInt32
  var userResizingAllowed: UInt32
  var autoResizingAllowed: UInt32
  var hidden: UInt32
  var RESERVED: UInt32
  init()
  init(oldIsResizable oldIsResizable: UInt32, isEditable isEditable: UInt32, resizedPostingDisableCount resizedPostingDisableCount: UInt32, canUseReorderResizeImageCache canUseReorderResizeImageCache: UInt32, userResizingAllowed userResizingAllowed: UInt32, autoResizingAllowed autoResizingAllowed: UInt32, hidden hidden: UInt32, RESERVED RESERVED: UInt32)
}
extension NSTableColumn {
  var dataCell: AnyObject
  @discardableResult
  func dataCell(forRow row: Int) -> AnyObject
}
