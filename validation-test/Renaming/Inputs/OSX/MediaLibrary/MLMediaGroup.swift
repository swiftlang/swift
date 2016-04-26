
class MLMediaGroup : NSObject {
  unowned(unsafe) var mediaLibrary: @sil_unmanaged MLMediaLibrary? { get }
  unowned(unsafe) var parent: @sil_unmanaged MLMediaGroup? { get }
  var mediaSourceIdentifier: String { get }
  var name: String? { get }
  var identifier: String { get }
  var typeIdentifier: String { get }
  var attributes: [String : AnyObject] { get }
  var childGroups: [MLMediaGroup]? { get }
  @NSCopying var url: NSURL? { get }
  @NSCopying var modificationDate: NSDate? { get }
  var mediaObjects: [MLMediaObject]? { get }
}
