
class MLMediaObject : NSObject {
  unowned(unsafe) var mediaLibrary: @sil_unmanaged MLMediaLibrary? { get }
  var identifier: String { get }
  var mediaSourceIdentifier: String { get }
  var attributes: [String : AnyObject] { get }
  var mediaType: MLMediaType { get }
  var contentType: String? { get }
  var name: String? { get }
  @NSCopying var url: NSURL? { get }
  @NSCopying var originalURL: NSURL? { get }
  var fileSize: Int { get }
  @NSCopying var modificationDate: NSDate? { get }
  @NSCopying var thumbnailURL: NSURL? { get }
}
