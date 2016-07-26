
@available(iOS 8.0, *)
class WKWebView : UIView {
  @NSCopying var configuration: WKWebViewConfiguration { get }
  weak var navigationDelegate: @sil_weak WKNavigationDelegate?
  weak var uiDelegate: @sil_weak WKUIDelegate?
  var backForwardList: WKBackForwardList { get }
  init(frame frame: CGRect, configuration configuration: WKWebViewConfiguration)
  @discardableResult
  func load(_ request: NSURLRequest) -> WKNavigation?
  @available(iOS 9.0, *)
  @discardableResult
  func loadFileURL(_ URL: NSURL, allowingReadAccessTo readAccessURL: NSURL) -> WKNavigation?
  @discardableResult
  func loadHTMLString(_ string: String, baseURL baseURL: NSURL?) -> WKNavigation?
  @available(iOS 9.0, *)
  @discardableResult
  func load(_ data: NSData, mimeType MIMEType: String, characterEncodingName characterEncodingName: String, baseURL baseURL: NSURL) -> WKNavigation?
  @discardableResult
  func go(to item: WKBackForwardListItem) -> WKNavigation?
  var title: String? { get }
  @NSCopying var url: NSURL? { get }
  var isLoading: Bool { get }
  var estimatedProgress: Double { get }
  var hasOnlySecureContent: Bool { get }
  @available(iOS 9.0, *)
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
  @available(iOS 9.0, *)
  var customUserAgent: String?
  @available(iOS 9.0, *)
  var allowsLinkPreview: Bool
  var scrollView: UIScrollView { get }
}
