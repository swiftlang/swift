
protocol WKScriptMessageHandler : NSObjectProtocol {
  @available(iOS 8.0, *)
  func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage)
}
