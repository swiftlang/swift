
let ADErrorDomain: String
@available(iOS 4.0, *)
enum ADError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case serverFailure
  case loadingThrottled
  case inventoryUnavailable
  case configurationError
  case bannerVisibleWithoutContent
  case applicationInactive
  case adUnloaded
  case assetLoadFailure
}
@available(iOS 6.0, *)
enum ADAdType : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case banner
  case mediumRectangle
}
@available(iOS 4.0, *)
class ADBannerView : UIView {
  @available(iOS 6.0, *)
  init!(adType type: ADAdType)
  @available(iOS 6.0, *)
  var adType: ADAdType { get }
  weak var delegate: @sil_weak ADBannerViewDelegate!
  var isBannerLoaded: Bool { get }
  var isBannerViewActionInProgress: Bool { get }
  func cancelAction()
  var advertisingSection: String!
}
protocol ADBannerViewDelegate : NSObjectProtocol {
  @available(iOS 5.0, *)
  optional func bannerViewWillLoadAd(_ banner: ADBannerView!)
  @available(iOS 4.0, *)
  optional func bannerViewDidLoadAd(_ banner: ADBannerView!)
  @available(iOS 4.0, *)
  optional func bannerView(_ banner: ADBannerView!, didFailToReceiveAdWithError error: NSError!)
  @available(iOS 4.0, *)
  @discardableResult
  optional func bannerViewActionShouldBegin(_ banner: ADBannerView!, willLeaveApplication willLeave: Bool) -> Bool
  @available(iOS 4.0, *)
  optional func bannerViewActionDidFinish(_ banner: ADBannerView!)
}
