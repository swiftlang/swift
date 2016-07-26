
@available(OSX 10.5, *)
class NSPathControl : NSControl {
  @available(OSX 10.10, *)
  var isEditable: Bool
  @available(OSX 10.10, *)
  var allowedTypes: [String]?
  @available(OSX 10.10, *)
  var placeholderString: String?
  @available(OSX 10.10, *)
  @NSCopying var placeholderAttributedString: NSAttributedString?
  @NSCopying var url: NSURL?
  var doubleAction: Selector?
  var pathStyle: NSPathStyle
  @available(OSX 10.10, *)
  var clickedPathItem: NSPathControlItem? { get }
  @available(OSX 10.10, *)
  var pathItems: [NSPathControlItem]
  @NSCopying var backgroundColor: NSColor?
  weak var delegate: @sil_weak NSPathControlDelegate?
  func setDraggingSourceOperationMask(_ mask: NSDragOperation, forLocal isLocal: Bool)
}
protocol NSPathControlDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  @discardableResult
  optional func pathControl(_ pathControl: NSPathControl, shouldDrag pathItem: NSPathControlItem, with pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func pathControl(_ pathControl: NSPathControl, shouldDrag pathComponentCell: NSPathComponentCell, with pasteboard: NSPasteboard) -> Bool
  @available(OSX 10.5, *)
  @discardableResult
  optional func pathControl(_ pathControl: NSPathControl, validateDrop info: NSDraggingInfo) -> NSDragOperation
  @available(OSX 10.5, *)
  @discardableResult
  optional func pathControl(_ pathControl: NSPathControl, acceptDrop info: NSDraggingInfo) -> Bool
  @available(OSX 10.5, *)
  optional func pathControl(_ pathControl: NSPathControl, willDisplay openPanel: NSOpenPanel)
  @available(OSX 10.5, *)
  optional func pathControl(_ pathControl: NSPathControl, willPopUp menu: NSMenu)
}
extension NSPathControl {
  @discardableResult
  func clickedPathComponentCell() -> NSPathComponentCell?
  @discardableResult
  func pathComponentCells() -> [NSPathComponentCell]
  func setPathComponentCells(_ cells: [NSPathComponentCell])
}
