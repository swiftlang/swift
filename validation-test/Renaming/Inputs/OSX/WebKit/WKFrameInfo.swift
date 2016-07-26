
@available(OSX 10.10, *)
class WKFrameInfo : NSObject, NSCopying {
  var isMainFrame: Bool { get }
  @NSCopying var request: NSURLRequest { get }
  @available(OSX 10.11, *)
  var securityOrigin: WKSecurityOrigin { get }
}
