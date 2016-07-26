
protocol UIDocumentPickerDelegate : NSObjectProtocol {
  @available(iOS 8.0, *)
  func documentPicker(_ controller: UIDocumentPickerViewController, didPickDocumentAt url: NSURL)
  @available(iOS 8.0, *)
  optional func documentPickerWasCancelled(_ controller: UIDocumentPickerViewController)
}
@available(iOS 8.0, *)
enum UIDocumentPickerMode : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `import`
  case open
  case exportToService
  case moveToService
}
@available(iOS 8.0, *)
class UIDocumentPickerViewController : UIViewController {
  init(documentTypes allowedUTIs: [String], in mode: UIDocumentPickerMode)
  init(url url: NSURL, in mode: UIDocumentPickerMode)
  weak var delegate: @sil_weak UIDocumentPickerDelegate?
  var documentPickerMode: UIDocumentPickerMode { get }
}
