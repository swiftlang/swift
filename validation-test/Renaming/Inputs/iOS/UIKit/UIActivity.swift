
@available(iOS 6.0, *)
let UIActivityTypePostToFacebook: String
@available(iOS 6.0, *)
let UIActivityTypePostToTwitter: String
@available(iOS 6.0, *)
let UIActivityTypePostToWeibo: String
@available(iOS 6.0, *)
let UIActivityTypeMessage: String
@available(iOS 6.0, *)
let UIActivityTypeMail: String
@available(iOS 6.0, *)
let UIActivityTypePrint: String
@available(iOS 6.0, *)
let UIActivityTypeCopyToPasteboard: String
@available(iOS 6.0, *)
let UIActivityTypeAssignToContact: String
@available(iOS 6.0, *)
let UIActivityTypeSaveToCameraRoll: String
@available(iOS 7.0, *)
let UIActivityTypeAddToReadingList: String
@available(iOS 7.0, *)
let UIActivityTypePostToFlickr: String
@available(iOS 7.0, *)
let UIActivityTypePostToVimeo: String
@available(iOS 7.0, *)
let UIActivityTypePostToTencentWeibo: String
@available(iOS 7.0, *)
let UIActivityTypeAirDrop: String
@available(iOS 9.0, *)
let UIActivityTypeOpenInIBooks: String
@available(iOS 7.0, *)
enum UIActivityCategory : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case action
  case share
}
@available(iOS 6.0, *)
class UIActivity : NSObject {
  @available(iOS 7.0, *)
  @discardableResult
  class func activityCategory() -> UIActivityCategory
  @discardableResult
  func activityType() -> String?
  @discardableResult
  func activityTitle() -> String?
  @discardableResult
  func activityImage() -> UIImage?
  @discardableResult
  func canPerform(withActivityItems activityItems: [AnyObject]) -> Bool
  func prepare(withActivityItems activityItems: [AnyObject])
  @discardableResult
  func activityViewController() -> UIViewController?
  func perform()
  func activityDidFinish(_ completed: Bool)
}
