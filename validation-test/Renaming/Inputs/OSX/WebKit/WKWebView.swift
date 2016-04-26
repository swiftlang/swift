
@available(OSX 10.10, *)
class WKWebView : NSView {
  @NSCopying var configuration: WKWebViewConfiguration { get }
  weak var navigationDelegate: @sil_weak WKNavigationDelegate?
  weak var uiDelegate: @sil_weak WKUIDelegate?
  var backForwardList: WKBackForwardList { get }
  init(frame frame: CGRect, configuration configuration: WKWebViewConfiguration)
  @discardableResult
  func load(_ request: NSURLRequest) -> WKNavigation?
  @available(OSX 10.11, *)
  @discardableResult
  func loadFileURL(_ URL: NSURL, allowingReadAccessTo readAccessURL: NSURL) -> WKNavigation?
  @discardableResult
  func loadHTMLString(_ string: String, baseURL baseURL: NSURL?) -> WKNavigation?
  @available(OSX 10.11, *)
  @discardableResult
  func load(_ data: NSData, mimeType MIMEType: String, characterEncodingName characterEncodingName: String, baseURL baseURL: NSURL) -> WKNavigation?
  @discardableResult
  func go(to item: WKBackForwardListItem) -> WKNavigation?
  var title: String? { get }
  @NSCopying var url: NSURL? { get }
  var isLoading: Bool { get }
  var estimatedProgress: Double { get }
  var hasOnlySecureContent: Bool { get }
  @available(OSX 10.11, *)
  var certificateChain: [AnyObject] { get }
  var canGoBack: Bool { get }
  var canGoForward: Bool { get }
  @discardableResult
  func goBack() -> WKNavigation?
  @discardableResult
  func goForward() -> WKNavigation?
  @discardableResult
  func reload() -> WKNavigation?
  @discardableResult
  func reloadFromOrigin() -> WKNavigation?
  func stopLoading()
  func evaluateJavaScript(_ javaScriptString: String, completionHandler completionHandler: ((AnyObject?, NSError?) -> Void)? = nil)
  var allowsBackForwardNavigationGestures: Bool
  @available(OSX 10.11, *)
  var customUserAgent: String?
  @available(OSX 10.11, *)
  var allowsLinkPreview: Bool
  var allowsMagnification: Bool
  var magnification: CGFloat
  func setMagnification(_ magnification: CGFloat, centeredAt point: CGPoint)
}
extension WKWebView : NSUserInterfaceValidations {
  @IBAction func goBack(_ sender: AnyObject?)
  @IBAction func goForward(_ sender: AnyObject?)
  @IBAction func reload(_ sender: AnyObject?)
  @IBAction func reloadFromOrigin(_ sender: AnyObject?)
  @IBAction func stopLoading(_ sender: AnyObject?)
}
