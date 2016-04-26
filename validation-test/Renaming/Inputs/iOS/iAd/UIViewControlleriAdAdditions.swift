
@available(iOS 7.0, *)
enum ADInterstitialPresentationPolicy : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case none
  case automatic
  case manual
}
extension UIViewController {
  @available(iOS 7.0, *)
  class func prepareInterstitialAds()
  @available(iOS 7.0, *)
  var interstitialPresentationPolicy: ADInterstitialPresentationPolicy
  @available(iOS 7.0, *)
  var canDisplayBannerAds: Bool
  @available(iOS 7.0, *)
  var originalContentView: UIView! { get }
  @available(iOS 7.0, *)
  var isPresentingFullScreenAd: Bool { get }
  @available(iOS 7.0, *)
  var isDisplayingBannerAd: Bool { get }
  @available(iOS 7.0, *)
  @discardableResult
  func requestInterstitialAdPresentation() -> Bool
  @available(iOS 7.0, *)
  var shouldPresentInterstitialAd: Bool { get }
}
