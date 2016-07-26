
@available(OSX 10.10, *)
class CKAsset : NSObject {
  init(fileURL fileURL: NSURL)
  @NSCopying var fileURL: NSURL { get }
}
