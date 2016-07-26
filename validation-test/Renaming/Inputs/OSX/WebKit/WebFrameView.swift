
class WebFrameView : NSView {
  var documentView: NSView! { get }
  var allowsScrolling: Bool
  var canPrintHeadersAndFooters: Bool { get }
  @discardableResult
  func printOperation(with printInfo: NSPrintInfo!) -> NSPrintOperation!
  var documentViewShouldHandlePrint: Bool { get }
  func printDocumentView()
}
