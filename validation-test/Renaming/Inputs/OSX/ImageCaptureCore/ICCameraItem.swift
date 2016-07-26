
class ICCameraItem : NSObject {
  var device: ICCameraDevice { get }
  var parentFolder: ICCameraFolder { get }
  var name: String { get }
  var uti: String { get }
  var fileSystemPath: String { get }
  var isLocked: Bool { get }
  var isRaw: Bool { get }
  var isInTemporaryStore: Bool { get }
  var creationDate: NSDate { get }
  var modificationDate: NSDate { get }
  var thumbnailIfAvailable: CGImage? { get }
  var largeThumbnailIfAvailable: CGImage? { get }
  var metadataIfAvailable: [String : AnyObject]? { get }
  var userData: NSMutableDictionary? { get }
  var ptpObjectHandle: UInt32 { get }
  var wasAddedAfterContentCatalogCompleted: Bool { get }
}
class ICCameraFolder : ICCameraItem {
  var contents: [ICCameraItem] { get }
}
class ICCameraFile : ICCameraItem {
  var fileSize: off_t { get }
  var orientation: ICEXIFOrientationType
  var duration: Double { get }
  var sidecarFiles: [ICCameraItem] { get }
}
