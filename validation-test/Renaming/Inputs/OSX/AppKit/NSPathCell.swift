
enum NSPathStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case standard
  case popUp
}
@available(OSX 10.5, *)
class NSPathCell : NSActionCell, NSOpenSavePanelDelegate {
  var pathStyle: NSPathStyle
  @NSCopying var url: NSURL?
  var allowedTypes: [String]?
  unowned(unsafe) var delegate: @sil_unmanaged NSPathCellDelegate?
  @discardableResult
  class func pathComponentCellClass() -> AnyClass
  var pathComponentCells: [NSPathComponentCell]
  @discardableResult
  func rect(of cell: NSPathComponentCell, withFrame frame: NSRect, in view: NSView) -> NSRect
  @discardableResult
  func pathComponentCell(at point: NSPoint, withFrame frame: NSRect, in view: NSView) -> NSPathComponentCell?
  var clickedPathComponentCell: NSPathComponentCell? { get }
  func mouseEntered(_ event: NSEvent, withFrame frame: NSRect, in view: NSView)
  func mouseExited(_ event: NSEvent, withFrame frame: NSRect, in view: NSView)
  var doubleAction: Selector?
  @NSCopying var backgroundColor: NSColor?
  var placeholderString: String?
  @NSCopying var placeholderAttributedString: NSAttributedString?
}
protocol NSPathCellDelegate : NSObjectProtocol {
  @available(OSX 10.5, *)
  optional func pathCell(_ pathCell: NSPathCell, willDisplay openPanel: NSOpenPanel)
  @available(OSX 10.5, *)
  optional func pathCell(_ pathCell: NSPathCell, willPopUp menu: NSMenu)
}
