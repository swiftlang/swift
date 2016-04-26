
@available(OSX 10.10, *)
class WKScriptMessage : NSObject {
  @NSCopying var body: AnyObject { get }
  weak var webView: @sil_weak WKWebView? { get }
  @NSCopying var frameInfo: WKFrameInfo { get }
  var name: String { get }
}
