
@available(iOS 4.3, *)
class ADInterstitialAd : NSObject {
  weak var delegate: @sil_weak ADInterstitialAdDelegate!
  var isLoaded: Bool { get }
  var isActionInProgress: Bool { get }
  func cancelAction()
  @discardableResult
  func present(in containerView: UIView!) -> Bool
}
protocol ADInterstitialAdDelegate : NSObjectProtocol {
  @available(iOS 4.3, *)
  func interstitialAdDidUnload(_ interstitialAd: ADInterstitialAd!)
  @available(iOS 4.3, *)
  func interstitialAd(_ interstitialAd: ADInterstitialAd!, didFailWithError error: NSError!)
  @available(iOS 5.0, *)
  optional func interstitialAdWillLoad(_ interstitialAd: ADInterstitialAd!)
  @available(iOS 4.3, *)
  optional func interstitialAdDidLoad(_ interstitialAd: ADInterstitialAd!)
  @available(iOS 4.3, *)
  @discardableResult
  optional func interstitialAdActionShouldBegin(_ interstitialAd: ADInterstitialAd!, willLeaveApplication willLeave: Bool) -> Bool
  @available(iOS 4.3, *)
  optional func interstitialAdActionDidFinish(_ interstitialAd: ADInterstitialAd!)
}
