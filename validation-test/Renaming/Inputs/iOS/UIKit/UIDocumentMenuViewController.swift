
@available(iOS 8.0, *)
enum UIDocumentMenuOrder : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case first
  case last
}
protocol UIDocumentMenuDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  func documentMenu(_ documentMenu: UIDocumentMenuViewController, didPickDocumentPicker documentPicker: UIDocumentPickerViewController)
  @available(iOS 8.0, *)
  optional func documentMenuWasCancelled(_ documentMenu: UIDocumentMenuViewController)
}
@available(iOS 8.0, *)
class UIDocumentMenuViewController : UIViewController {
  init(documentTypes allowedUTIs: [String], in mode: UIDocumentPickerMode)
  init(url url: NSURL, in mode: UIDocumentPickerMode)
  func addOption(withTitle title: String, image image: UIImage?, order order: UIDocumentMenuOrder, handler handler: () -> Void)
  weak var delegate: @sil_weak UIDocumentMenuDelegate?
}
