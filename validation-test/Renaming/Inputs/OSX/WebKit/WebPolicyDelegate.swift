
enum WebNavigationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case linkClicked
  case formSubmitted
  case backForward
  case reload
  case formResubmitted
  case other
}
let WebActionNavigationTypeKey: String
let WebActionElementKey: String
let WebActionButtonKey: String
let WebActionModifierFlagsKey: String
let WebActionOriginalURLKey: String
protocol WebPolicyDecisionListener : NSObjectProtocol {
  func use()
  func download()
  func ignore()
}
protocol WebPolicyDelegate : NSObjectProtocol {
  @available(OSX 10.0, *)
  optional func webView(_ webView: WebView!, decidePolicyForNavigationAction actionInformation: [NSObject : AnyObject]!, request request: NSURLRequest!, frame frame: WebFrame!, decisionListener listener: WebPolicyDecisionListener!)
  @available(OSX 10.0, *)
  optional func webView(_ webView: WebView!, decidePolicyForNewWindowAction actionInformation: [NSObject : AnyObject]!, request request: NSURLRequest!, newFrameName frameName: String!, decisionListener listener: WebPolicyDecisionListener!)
  optional func webView(_ webView: WebView!, decidePolicyForMIMEType type: String!, request request: NSURLRequest!, frame frame: WebFrame!, decisionListener listener: WebPolicyDecisionListener!)
  optional func webView(_ webView: WebView!, unableToImplementPolicyWithError error: NSError!, frame frame: WebFrame!)
}
