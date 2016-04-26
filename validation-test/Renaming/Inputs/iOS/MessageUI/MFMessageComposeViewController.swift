
struct MessageComposeResult : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var MessageComposeResultCancelled: MessageComposeResult { get }
var MessageComposeResultSent: MessageComposeResult { get }
var MessageComposeResultFailed: MessageComposeResult { get }
let MFMessageComposeViewControllerAttachmentURL: String
let MFMessageComposeViewControllerAttachmentAlternateFilename: String
@available(iOS 5.0, *)
let MFMessageComposeViewControllerTextMessageAvailabilityDidChangeNotification: String
@available(iOS 5.0, *)
let MFMessageComposeViewControllerTextMessageAvailabilityKey: String
@available(iOS 4.0, *)
class MFMessageComposeViewController : UINavigationController {
  @available(iOS 4.0, *)
  @discardableResult
  class func canSendText() -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  class func canSendSubject() -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  class func canSendAttachments() -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  class func isSupportedAttachmentUTI(_ uti: String) -> Bool
  unowned(unsafe) var messageComposeDelegate: @sil_unmanaged MFMessageComposeViewControllerDelegate?
  @available(iOS 7.0, *)
  func disableUserAttachments()
  var recipients: [String]?
  var body: String?
  var subject: String?
  var attachments: [[NSObject : AnyObject]]? { get }
  @available(iOS 7.0, *)
  @discardableResult
  func addAttachmentURL(_ attachmentURL: NSURL, withAlternateFilename alternateFilename: String?) -> Bool
  @available(iOS 7.0, *)
  @discardableResult
  func addAttachmentData(_ attachmentData: NSData, typeIdentifier uti: String, filename filename: String) -> Bool
}
protocol MFMessageComposeViewControllerDelegate : NSObjectProtocol {
  @available(iOS 4.0, *)
  func messageComposeViewController(_ controller: MFMessageComposeViewController, didFinishWith result: MessageComposeResult)
}
