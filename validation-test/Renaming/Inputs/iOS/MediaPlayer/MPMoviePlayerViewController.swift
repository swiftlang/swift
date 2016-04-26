
@available(iOS 3.2, *)
@available(iOS, introduced: 3.2, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
class MPMoviePlayerViewController : UIViewController {
  init!(contentURL contentURL: NSURL!)
  var moviePlayer: MPMoviePlayerController! { get }
}
extension UIViewController {
  @available(iOS, introduced: 3.2, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  func presentMoviePlayerViewControllerAnimated(_ moviePlayerViewController: MPMoviePlayerViewController!)
  @available(iOS, introduced: 3.2, deprecated: 9.0, message: "Use AVPlayerViewController in AVKit.")
  func dismissMoviePlayerViewControllerAnimated()
}
