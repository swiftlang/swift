
struct MFMailComposeResult : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var MFMailComposeResultCancelled: MFMailComposeResult { get }
var MFMailComposeResultSaved: MFMailComposeResult { get }
var MFMailComposeResultSent: MFMailComposeResult { get }
var MFMailComposeResultFailed: MFMailComposeResult { get }
@available(iOS 3.0, *)
let MFMailComposeErrorDomain: String
struct MFMailComposeErrorCode : RawRepresentable, Equatable {
  init(_ rawValue: UInt32)
  init(rawValue rawValue: UInt32)
  var rawValue: UInt32
}
var MFMailComposeErrorCodeSaveFailed: MFMailComposeErrorCode { get }
var MFMailComposeErrorCodeSendFailed: MFMailComposeErrorCode { get }
class MFMailComposeViewController : UINavigationController {
  @available(iOS 3.0, *)
  @discardableResult
  class func canSendMail() -> Bool
  unowned(unsafe) var mailComposeDelegate: @sil_unmanaged MFMailComposeViewControllerDelegate?
  @available(iOS 3.0, *)
  func setSubject(_ subject: String)
  @available(iOS 3.0, *)
  func setToRecipients(_ toRecipients: [String]?)
  @available(iOS 3.0, *)
  func setCcRecipients(_ ccRecipients: [String]?)
  @available(iOS 3.0, *)
  func setBccRecipients(_ bccRecipients: [String]?)
  @available(iOS 3.0, *)
  func setMessageBody(_ body: String, isHTML isHTML: Bool)
  @available(iOS 3.0, *)
  func addAttachmentData(_ attachment: NSData, mimeType mimeType: String, fileName filename: String)
}
protocol MFMailComposeViewControllerDelegate : NSObjectProtocol {
  @available(iOS 3.0, *)
  optional func mailComposeController(_ controller: MFMailComposeViewController, didFinishWith result: MFMailComposeResult, error error: NSError?)
}
