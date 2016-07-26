
@available(OSX 10.10, *)
class WKWebViewConfiguration : NSObject, NSCopying {
  var processPool: WKProcessPool
  var preferences: WKPreferences
  var userContentController: WKUserContentController
  @available(OSX 10.11, *)
  var websiteDataStore: WKWebsiteDataStore
  var suppressesIncrementalRendering: Bool
  @available(OSX 10.11, *)
  var applicationNameForUserAgent: String?
  @available(OSX 10.11, *)
  var allowsAirPlayForMediaPlayback: Bool
}
extension WKWebViewConfiguration {
}
