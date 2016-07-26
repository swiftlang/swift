
class NSTableHeaderView : NSView {
  unowned(unsafe) var tableView: @sil_unmanaged NSTableView?
  var draggedColumn: Int { get }
  var draggedDistance: CGFloat { get }
  var resizedColumn: Int { get }
  @discardableResult
  func headerRect(ofColumn column: Int) -> NSRect
  @discardableResult
  func column(at point: NSPoint) -> Int
}
