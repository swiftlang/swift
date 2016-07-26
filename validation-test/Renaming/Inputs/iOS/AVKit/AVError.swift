
@available(iOS 9.0, *)
let AVKitErrorDomain: String
@available(iOS 9.0, *)
enum AVKitError : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknown
  case pictureInPictureStartFailed
}
