
@available(iOS 8.0, *)
enum WKNavigationActionPolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case cancel
  case allow
}
@available(iOS 8.0, *)
enum WKNavigationResponsePolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case cancel
  case allow
}
protocol WKNavigationDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler decisionHandler: (WKNavigationActionPolicy) -> Void)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, decidePolicyFor navigationResponse: WKNavigationResponse, decisionHandler decisionHandler: (WKNavigationResponsePolicy) -> Void)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, didReceiveServerRedirectForProvisionalNavigation navigation: WKNavigation!)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: NSError)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: NSError)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, didReceive challenge: NSURLAuthenticationChallenge, completionHandler completionHandler: (NSURLSessionAuthChallengeDisposition, NSURLCredential?) -> Void)
  @available(iOS 9.0, *)
  optional func webViewWebContentProcessDidTerminate(_ webView: WKWebView)
}
