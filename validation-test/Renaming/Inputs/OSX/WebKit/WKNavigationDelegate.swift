
@available(OSX 10.10, *)
enum WKNavigationActionPolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case cancel
  case allow
}
@available(OSX 10.10, *)
enum WKNavigationResponsePolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case cancel
  case allow
}
protocol WKNavigationDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler decisionHandler: (WKNavigationActionPolicy) -> Void)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, decidePolicyFor navigationResponse: WKNavigationResponse, decisionHandler decisionHandler: (WKNavigationResponsePolicy) -> Void)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, didReceiveServerRedirectForProvisionalNavigation navigation: WKNavigation!)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: NSError)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: NSError)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, didReceive challenge: NSURLAuthenticationChallenge, completionHandler completionHandler: (NSURLSessionAuthChallengeDisposition, NSURLCredential?) -> Void)
  @available(OSX 10.11, *)
  optional func webViewWebContentProcessDidTerminate(_ webView: WKWebView)
}
