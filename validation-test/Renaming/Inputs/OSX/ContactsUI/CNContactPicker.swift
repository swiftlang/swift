
@available(OSX 10.11, *)
class CNContactPicker : NSObject {
  var displayedKeys: [String]
  weak var delegate: @sil_weak CNContactPickerDelegate?
  func showRelative(to positioningRect: NSRect, of positioningView: NSView, preferredEdge preferredEdge: NSRectEdge)
  func close()
}
