
class WebFrame : NSObject {
  init!(name name: String!, webFrameView view: WebFrameView!, webView webView: WebView!)
  var name: String! { get }
  var webView: WebView! { get }
  var frameView: WebFrameView! { get }
  var domDocument: DOMDocument! { get }
  var frameElement: DOMHTMLElement! { get }
  func load(_ request: NSURLRequest!)
  func load(_ data: NSData!, mimeType MIMEType: String!, textEncodingName encodingName: String!, baseURL URL: NSURL!)
  func loadHTMLString(_ string: String!, baseURL URL: NSURL!)
  func loadAlternateHTMLString(_ string: String!, baseURL baseURL: NSURL!, forUnreachableURL unreachableURL: NSURL!)
  func load(_ archive: WebArchive!)
  var dataSource: WebDataSource? { get }
  var provisionalDataSource: WebDataSource! { get }
  func stopLoading()
  func reload()
  func reloadFromOrigin()
  @discardableResult
  func findNamed(_ name: String!) -> WebFrame!
  var parent: WebFrame! { get }
  var childFrames: [AnyObject]! { get }
  var windowObject: WebScriptObject! { get }
  var globalContext: JSGlobalContextRef! { get }
  var javaScriptContext: JSContext! { get }
}
