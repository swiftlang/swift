
typealias NSItemProviderCompletionHandler = (NSSecureCoding?, NSError!) -> Void
typealias NSItemProviderLoadHandler = (NSItemProviderCompletionHandler!, AnyClass!, [NSObject : AnyObject]!) -> Void
@available(tvOS 8.0, *)
class NSItemProvider : NSObject, NSCopying {
  init(item item: NSSecureCoding?, typeIdentifier typeIdentifier: String?)
  convenience init?(contentsOf fileURL: NSURL!)
  func registerItem(forTypeIdentifier typeIdentifier: String, loadHandler loadHandler: NSItemProviderLoadHandler)
  var registeredTypeIdentifiers: [AnyObject] { get }
  @discardableResult
  func hasItemConformingToTypeIdentifier(_ typeIdentifier: String) -> Bool
  func loadItem(forTypeIdentifier typeIdentifier: String, options options: [NSObject : AnyObject]? = [:], completionHandler completionHandler: NSItemProviderCompletionHandler? = nil)
}
@available(tvOS 8.0, *)
let NSItemProviderPreferredImageSizeKey: String
extension NSItemProvider {
  @available(tvOS 8.0, *)
  var previewImageHandler: NSItemProviderLoadHandler?
  @available(tvOS 8.0, *)
  func loadPreviewImage(options options: [NSObject : AnyObject]! = [:], completionHandler completionHandler: NSItemProviderCompletionHandler!)
}
@available(tvOS 8.0, *)
let NSExtensionJavaScriptPreprocessingResultsKey: String
@available(tvOS 8.0, *)
let NSExtensionJavaScriptFinalizeArgumentKey: String
@available(tvOS 8.0, *)
let NSItemProviderErrorDomain: String
@available(tvOS 8.0, *)
enum NSItemProviderErrorCode : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case unknownError
  case itemUnavailableError
  case unexpectedValueClassError
  @available(tvOS 9.0, *)
  case unavailableCoercionError
}
