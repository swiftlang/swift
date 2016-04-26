
@available(OSX 10.7, *)
class NSTableCellView : NSView {
  var objectValue: AnyObject?
  @IBOutlet unowned(unsafe) var textField: @sil_unmanaged NSTextField?
  @IBOutlet unowned(unsafe) var imageView: @sil_unmanaged NSImageView?
  var backgroundStyle: NSBackgroundStyle
  var rowSizeStyle: NSTableViewRowSizeStyle
  var draggingImageComponents: [NSDraggingImageComponent] { get }
}
