
@available(iOS 8.2, *)
class WKInterfaceTable : WKInterfaceObject {
  func setRowTypes(_ rowTypes: [String])
  func setNumberOfRows(_ numberOfRows: Int, withRowType rowType: String)
  var numberOfRows: Int { get }
  @discardableResult
  func rowController(at index: Int) -> AnyObject?
  func insertRows(at rows: NSIndexSet, withRowType rowType: String)
  func removeRows(at rows: NSIndexSet)
  func scrollToRow(at index: Int)
}
