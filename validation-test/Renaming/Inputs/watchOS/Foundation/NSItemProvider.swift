
typealias NSItemProviderCompletionHandler = (NSSecureCoding?, NSError!) -> Void
typealias NSItemProviderLoadHandler = (NSItemProviderCompletionHandler!, AnyClass!, [NSObject : AnyObject]!) -> Void
@available(watchOS 2.0, *)
class NSItemProvider : NSObject, NSCopying {
  init(item item: NSSecureCoding?, typeIdentifier typeIdentifier: String?)
  convenience init?(contentsOf fileURL: NSURL!)
  func registerItem(forTypeIdentifier typeIdentifier: String, loadHandler loadHandler: NSItemProviderLoadHandler)
  var registeredTypeIdentifiers: [AnyObject] { get }
  @discardableResult
  func hasItemConformingToTypeIdentifier(_ typeIdentifier: String) -> Bool
  func loadItem(forTypeIdentifier typeIdentifier: String, options options: [NSObject : AnyObject]? = [:], completionHandler completionHandler: NSItemProviderCompletionHandler? = nil)
}
@available(watchOS 2.0, *)
let NSItemProviderPreferredImageSizeKey: String
extension NSItemProvider {
  @available(watchOS 2.0, *)
  var previewImageHandler: NSItemProviderLoadHandler?
  @available(watchOS 2.0, *)
  func loadPreviewImage(options options: [NSObject : AnyObject]! = [:], completionHandler completionHandler: NSItemProviderCompletionHandler!)
}
@available(watchOS 2.0, *)
let NSExtensionJavaScriptPreprocessingResultsKey: String
@available(watchOS 2.0, *)
let NSExtensionJavaScriptFinalizeArgumentKey: String
@available(watchOS 2.0, *)
let NSItemProviderErrorDomain: String
@available(watchOS 2.0, *)
enum NSItemProviderErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknownError
  case itemUnavailableError
  case unexpectedValueClassError
  @available(watchOS 2.0, *)
  case unavailableCoercionError
}
