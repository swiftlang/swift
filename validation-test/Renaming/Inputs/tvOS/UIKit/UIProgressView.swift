
enum UIProgressViewStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case `default`
}
@available(tvOS 2.0, *)
class UIProgressView : UIView, NSCoding {
  convenience init(progressViewStyle style: UIProgressViewStyle)
  var progressViewStyle: UIProgressViewStyle
  var progress: Float
  @available(tvOS 5.0, *)
  var progressTintColor: UIColor?
  @available(tvOS 5.0, *)
  var trackTintColor: UIColor?
  @available(tvOS 5.0, *)
  var progressImage: UIImage?
  @available(tvOS 5.0, *)
  var trackImage: UIImage?
  @available(tvOS 5.0, *)
  func setProgress(_ progress: Float, animated animated: Bool)
  @available(tvOS 9.0, *)
  var observedProgress: NSProgress?
}
