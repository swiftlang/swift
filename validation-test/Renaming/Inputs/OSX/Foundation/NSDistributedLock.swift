
class NSDistributedLock : NSObject {
  init?(path path: String)
  @discardableResult
  func tryLock() -> Bool
  func unlock()
  func breakLock()
  @NSCopying var lockDate: NSDate { get }
}
