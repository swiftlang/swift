
@available(iOS 8.0, *)
class WKFrameInfo : NSObject, NSCopying {
  var isMainFrame: Bool { get }
  @NSCopying var request: NSURLRequest { get }
  @available(iOS 9.0, *)
  var securityOrigin: WKSecurityOrigin { get }
}
