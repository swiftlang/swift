
enum UIWebViewNavigationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case linkClicked
  case formSubmitted
  case backForward
  case reload
  case formResubmitted
  case other
}
enum UIWebPaginationMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unpaginated
  case leftToRight
  case topToBottom
  case bottomToTop
  case rightToLeft
}
enum UIWebPaginationBreakingMode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case page
  case column
}
@available(iOS 2.0, *)
class UIWebView : UIView, NSCoding, UIScrollViewDelegate {
  unowned(unsafe) var delegate: @sil_unmanaged UIWebViewDelegate?
  @available(iOS 5.0, *)
  var scrollView: UIScrollView { get }
  func loadRequest(_ request: NSURLRequest)
  func loadHTMLString(_ string: String, baseURL baseURL: NSURL?)
  func load(_ data: NSData, mimeType MIMEType: String, textEncodingName textEncodingName: String, baseURL baseURL: NSURL)
  var request: NSURLRequest? { get }
  func reload()
  func stopLoading()
  func goBack()
  func goForward()
  var canGoBack: Bool { get }
  var canGoForward: Bool { get }
  var isLoading: Bool { get }
  @discardableResult
  func stringByEvaluatingJavaScript(from script: String) -> String?
  var scalesPageToFit: Bool
  @available(iOS 3.0, *)
  var dataDetectorTypes: UIDataDetectorTypes
  @available(iOS 4.0, *)
  var allowsInlineMediaPlayback: Bool
  @available(iOS 4.0, *)
  var mediaPlaybackRequiresUserAction: Bool
  @available(iOS 5.0, *)
  var mediaPlaybackAllowsAirPlay: Bool
  @available(iOS 6.0, *)
  var suppressesIncrementalRendering: Bool
  @available(iOS 6.0, *)
  var keyboardDisplayRequiresUserAction: Bool
  @available(iOS 7.0, *)
  var paginationMode: UIWebPaginationMode
  @available(iOS 7.0, *)
  var paginationBreakingMode: UIWebPaginationBreakingMode
  @available(iOS 7.0, *)
  var pageLength: CGFloat
  @available(iOS 7.0, *)
  var gapBetweenPages: CGFloat
  @available(iOS 7.0, *)
  var pageCount: Int { get }
  @available(iOS 9.0, *)
  var allowsPictureInPictureMediaPlayback: Bool
  @available(iOS 9.0, *)
  var allowsLinkPreview: Bool
}
protocol UIWebViewDelegate : NSObjectProtocol {
  @available(iOS 2.0, *)
  @discardableResult
  optional func webView(_ webView: UIWebView, shouldStartLoadWith request: NSURLRequest, navigationType navigationType: UIWebViewNavigationType) -> Bool
  @available(iOS 2.0, *)
  optional func webViewDidStartLoad(_ webView: UIWebView)
  @available(iOS 2.0, *)
  optional func webViewDidFinishLoad(_ webView: UIWebView)
  @available(iOS 2.0, *)
  optional func webView(_ webView: UIWebView, didFailLoadWithError error: NSError?)
}
