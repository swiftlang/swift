
@available(iOS 8.0, *)
enum PHAuthorizationStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case notDetermined
  case restricted
  case denied
  case authorized
}
@available(iOS 8.0, *)
protocol PHPhotoLibraryChangeObserver : NSObjectProtocol {
  func photoLibraryDidChange(_ changeInstance: PHChange)
}
@available(iOS 8.0, *)
class PHPhotoLibrary : NSObject {
  @discardableResult
  class func shared() -> PHPhotoLibrary
  @discardableResult
  class func authorizationStatus() -> PHAuthorizationStatus
  class func requestAuthorization(_ handler: (PHAuthorizationStatus) -> Void)
  func performChanges(_ changeBlock: dispatch_block_t, completionHandler completionHandler: ((Bool, NSError?) -> Void)? = nil)
  func performChangesAndWait(_ changeBlock: dispatch_block_t) throws
  func register(_ observer: PHPhotoLibraryChangeObserver)
  func unregisterChangeObserver(_ observer: PHPhotoLibraryChangeObserver)
}
