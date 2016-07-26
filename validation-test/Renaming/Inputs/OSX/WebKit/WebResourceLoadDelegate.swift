
protocol WebResourceLoadDelegate : NSObjectProtocol {
  @discardableResult
  optional func webView(_ sender: WebView!, identifierForInitialRequest request: NSURLRequest!, from dataSource: WebDataSource!) -> AnyObject!
  @discardableResult
  optional func webView(_ sender: WebView!, resource identifier: AnyObject!, willSend request: NSURLRequest!, redirectResponse redirectResponse: NSURLResponse!, from dataSource: WebDataSource!) -> NSURLRequest!
  optional func webView(_ sender: WebView!, resource identifier: AnyObject!, didReceive challenge: NSURLAuthenticationChallenge!, from dataSource: WebDataSource!)
  optional func webView(_ sender: WebView!, resource identifier: AnyObject!, didCancel challenge: NSURLAuthenticationChallenge!, from dataSource: WebDataSource!)
  optional func webView(_ sender: WebView!, resource identifier: AnyObject!, didReceive response: NSURLResponse!, from dataSource: WebDataSource!)
  optional func webView(_ sender: WebView!, resource identifier: AnyObject!, didReceiveContentLength length: Int, from dataSource: WebDataSource!)
  optional func webView(_ sender: WebView!, resource identifier: AnyObject!, didFinishLoadingFrom dataSource: WebDataSource!)
  optional func webView(_ sender: WebView!, resource identifier: AnyObject!, didFailLoadingWithError error: NSError!, from dataSource: WebDataSource!)
  optional func webView(_ sender: WebView!, plugInFailedWithError error: NSError!, dataSource dataSource: WebDataSource!)
}
