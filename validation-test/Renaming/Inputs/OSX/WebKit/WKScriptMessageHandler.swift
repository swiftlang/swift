
protocol WKScriptMessageHandler : NSObjectProtocol {
  @available(OSX 10.10, *)
  func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage)
}
