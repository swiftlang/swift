
class MLMediaSource : NSObject {
  unowned(unsafe) var mediaLibrary: @sil_unmanaged MLMediaLibrary? { get }
  var mediaSourceIdentifier: String { get }
  var attributes: [String : AnyObject] { get }
  var rootMediaGroup: MLMediaGroup? { get }
  @discardableResult
  func mediaGroup(forIdentifier mediaGroupIdentifier: String) -> MLMediaGroup?
  @discardableResult
  func mediaGroups(forIdentifiers mediaGroupIdentifiers: [String]) -> [String : MLMediaGroup]
  @discardableResult
  func mediaObject(forIdentifier mediaObjectIdentifier: String) -> MLMediaObject?
  @discardableResult
  func mediaObjects(forIdentifiers mediaObjectIdentifiers: [String]) -> [String : MLMediaObject]
}
