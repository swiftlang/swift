
protocol WebFrameLoadDelegate : NSObjectProtocol {
  optional func webView(_ sender: WebView!, didStartProvisionalLoadFor frame: WebFrame!)
  optional func webView(_ sender: WebView!, didReceiveServerRedirectForProvisionalLoadFor frame: WebFrame!)
  optional func webView(_ sender: WebView!, didFailProvisionalLoadWithError error: NSError!, for frame: WebFrame!)
  optional func webView(_ sender: WebView!, didCommitLoadFor frame: WebFrame!)
  optional func webView(_ sender: WebView!, didReceiveTitle title: String!, for frame: WebFrame!)
  optional func webView(_ sender: WebView!, didReceiveIcon image: NSImage!, for frame: WebFrame!)
  optional func webView(_ sender: WebView!, didFinishLoadFor frame: WebFrame!)
  optional func webView(_ sender: WebView!, didFailLoadWithError error: NSError!, for frame: WebFrame!)
  optional func webView(_ sender: WebView!, didChangeLocationWithinPageFor frame: WebFrame!)
  optional func webView(_ sender: WebView!, willPerformClientRedirectTo URL: NSURL!, delay seconds: NSTimeInterval, fire date: NSDate!, for frame: WebFrame!)
  optional func webView(_ sender: WebView!, didCancelClientRedirectFor frame: WebFrame!)
  optional func webView(_ sender: WebView!, willClose frame: WebFrame!)
  @available(OSX 10.4, *)
  optional func webView(_ webView: WebView!, didClearWindowObject windowObject: WebScriptObject!, for frame: WebFrame!)
  @available(OSX 10.9, *)
  optional func webView(_ webView: WebView!, didCreateJavaScriptContext context: JSContext!, for frame: WebFrame!)
}
