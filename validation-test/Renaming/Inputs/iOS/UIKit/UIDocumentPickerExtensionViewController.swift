
@available(iOS 8.0, *)
class UIDocumentPickerExtensionViewController : UIViewController {
  func dismissGrantingAccess(to url: NSURL?)
  func prepareForPresentation(in mode: UIDocumentPickerMode)
  var documentPickerMode: UIDocumentPickerMode { get }
  @NSCopying var originalURL: NSURL? { get }
  var validTypes: [String]? { get }
  var providerIdentifier: String { get }
  @NSCopying var documentStorageURL: NSURL? { get }
}
