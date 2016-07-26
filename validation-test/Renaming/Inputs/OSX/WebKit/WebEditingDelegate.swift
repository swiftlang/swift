
enum WebViewInsertAction : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case typed
  case pasted
  case dropped
}
protocol WebEditingDelegate : NSObjectProtocol {
}
extension NSObject {
  @discardableResult
  class func webView(_ webView: WebView!, shouldBeginEditingIn range: DOMRange!) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldBeginEditingIn range: DOMRange!) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, shouldEndEditingIn range: DOMRange!) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldEndEditingIn range: DOMRange!) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, shouldInsert node: DOMNode!, replacing range: DOMRange!, given action: WebViewInsertAction) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldInsert node: DOMNode!, replacing range: DOMRange!, given action: WebViewInsertAction) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, shouldInsertText text: String!, replacing range: DOMRange!, given action: WebViewInsertAction) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldInsertText text: String!, replacing range: DOMRange!, given action: WebViewInsertAction) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, shouldDelete range: DOMRange!) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldDelete range: DOMRange!) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, shouldChangeSelectedDOMRange currentRange: DOMRange!, to proposedRange: DOMRange!, affinity selectionAffinity: NSSelectionAffinity, stillSelecting flag: Bool) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldChangeSelectedDOMRange currentRange: DOMRange!, to proposedRange: DOMRange!, affinity selectionAffinity: NSSelectionAffinity, stillSelecting flag: Bool) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, shouldApplyStyle style: DOMCSSStyleDeclaration!, toElementsIn range: DOMRange!) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldApplyStyle style: DOMCSSStyleDeclaration!, toElementsIn range: DOMRange!) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, shouldChangeTypingStyle currentStyle: DOMCSSStyleDeclaration!, toStyle proposedStyle: DOMCSSStyleDeclaration!) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, shouldChangeTypingStyle currentStyle: DOMCSSStyleDeclaration!, toStyle proposedStyle: DOMCSSStyleDeclaration!) -> Bool
  @discardableResult
  class func webView(_ webView: WebView!, doCommandBy selector: Selector!) -> Bool
  @discardableResult
  func webView(_ webView: WebView!, doCommandBy selector: Selector!) -> Bool
  class func webViewDidBeginEditing(_ notification: NSNotification!)
  func webViewDidBeginEditing(_ notification: NSNotification!)
  class func webViewDidChange(_ notification: NSNotification!)
  func webViewDidChange(_ notification: NSNotification!)
  class func webViewDidEndEditing(_ notification: NSNotification!)
  func webViewDidEndEditing(_ notification: NSNotification!)
  class func webViewDidChangeTypingStyle(_ notification: NSNotification!)
  func webViewDidChangeTypingStyle(_ notification: NSNotification!)
  class func webViewDidChangeSelection(_ notification: NSNotification!)
  func webViewDidChangeSelection(_ notification: NSNotification!)
  @discardableResult
  class func undoManager(for webView: WebView!) -> NSUndoManager!
  @discardableResult
  func undoManager(for webView: WebView!) -> NSUndoManager!
}
