
@available(iOS 8.0, *)
enum WKNavigationType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case linkActivated
  case formSubmitted
  case backForward
  case reload
  case formResubmitted
  case other
}
@available(iOS 8.0, *)
class WKNavigationAction : NSObject {
  @NSCopying var sourceFrame: WKFrameInfo { get }
  @NSCopying var targetFrame: WKFrameInfo? { get }
  var navigationType: WKNavigationType { get }
  @NSCopying var request: NSURLRequest { get }
}
