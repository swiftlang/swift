
extension MPMoviePlayerController {
  @available(iOS 7.0, *)
  class func preparePrerollAds()
  @available(iOS 7.0, *)
  func playPrerollAd(completionHandler completionHandler: ((NSError!) -> Void)!)
  @available(iOS 8.0, *)
  func cancelPreroll()
}
