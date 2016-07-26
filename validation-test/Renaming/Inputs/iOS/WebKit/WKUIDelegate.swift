
protocol WKUIDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  @discardableResult
  optional func webView(_ webView: WKWebView, createWebViewWith configuration: WKWebViewConfiguration, for navigationAction: WKNavigationAction, windowFeatures windowFeatures: WKWindowFeatures) -> WKWebView?
  @available(iOS 9.0, *)
  optional func webViewDidClose(_ webView: WKWebView)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, runJavaScriptAlertPanelWithMessage message: String, initiatedByFrame frame: WKFrameInfo, completionHandler completionHandler: () -> Void)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, runJavaScriptConfirmPanelWithMessage message: String, initiatedByFrame frame: WKFrameInfo, completionHandler completionHandler: (Bool) -> Void)
  @available(iOS 8.0, *)
  optional func webView(_ webView: WKWebView, runJavaScriptTextInputPanelWithPrompt prompt: String, defaultText defaultText: String?, initiatedByFrame frame: WKFrameInfo, completionHandler completionHandler: (String?) -> Void)
}
