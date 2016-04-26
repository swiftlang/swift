
protocol WKUIDelegate : NSObjectProtocol {
  @available(OSX 10.10, *)
  @discardableResult
  optional func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration, for navigationAction: WKNavigationAction, windowFeatures windowFeatures: WKWindowFeatures) -> WKWebView?
  @available(OSX 10.11, *)
  optional func webViewDidClose(_ webView: WKWebView)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, runJavaScriptAlertPanelWithMessage message: String, initiatedByFrame frame: WKFrameInfo, completionHandler completionHandler: () -> Void)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, runJavaScriptConfirmPanelWithMessage message: String, initiatedByFrame frame: WKFrameInfo, completionHandler completionHandler: (Bool) -> Void)
  @available(OSX 10.10, *)
  optional func webView(_ webView: WKWebView, runJavaScriptTextInputPanelWithPrompt prompt: String, defaultText defaultText: String?, initiatedByFrame frame: WKFrameInfo, completionHandler completionHandler: (String?) -> Void)
}
