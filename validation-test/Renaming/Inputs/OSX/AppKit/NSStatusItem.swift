
class NSStatusItem : NSObject {
  unowned(unsafe) var statusBar: @sil_unmanaged NSStatusBar { get }
  var length: CGFloat
  var menu: NSMenu?
  @available(OSX 10.10, *)
  var button: NSStatusBarButton? { get }
}
extension NSStatusItem {
  var action: Selector?
  var doubleAction: Selector?
  weak var target: @sil_weak AnyObject?
  var title: String?
  @NSCopying var attributedTitle: NSAttributedString?
  var image: NSImage?
  var alternateImage: NSImage?
  var isEnabled: Bool
  var highlightMode: Bool
  var toolTip: String?
  @discardableResult
  func sendAction(on mask: Int) -> Int
  var view: NSView?
  func drawStatusBarBackground(in rect: NSRect, withHighlight highlight: Bool)
  func popUpMenu(_ menu: NSMenu)
}
