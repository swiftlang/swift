
var NSAppKitVersionNumberWithDockTilePlugInSupport: Double { get }
@available(OSX 10.5, *)
class NSDockTile : NSObject {
  var size: NSSize { get }
  var contentView: NSView?
  func display()
  var showsApplicationBadge: Bool
  var badgeLabel: String?
  unowned(unsafe) var owner: @sil_unmanaged AnyObject { get }
}
protocol NSDockTilePlugIn : NSObjectProtocol {
  @available(OSX 10.5, *)
  func setDockTile(_ dockTile: NSDockTile?)
  @discardableResult
  optional func dockMenu() -> NSMenu?
}
