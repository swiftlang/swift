
protocol NSLocking {
  func lock()
  func unlock()
}
class NSLock : NSObject, NSLocking {
  @discardableResult
  func tryLock() -> Bool
  @discardableResult
  func lock(before limit: NSDate) -> Bool
  @available(watchOS 2.0, *)
  var name: String?
}
class NSConditionLock : NSObject, NSLocking {
  init(condition condition: Int)
  var condition: Int { get }
  func lock(whenCondition condition: Int)
  @discardableResult
  func tryLock() -> Bool
  @discardableResult
  func tryLock(whenCondition condition: Int) -> Bool
  func unlock(withCondition condition: Int)
  @discardableResult
  func lock(before limit: NSDate) -> Bool
  @discardableResult
  func lock(whenCondition condition: Int, before limit: NSDate) -> Bool
  @available(watchOS 2.0, *)
  var name: String?
}
class NSRecursiveLock : NSObject, NSLocking {
  @discardableResult
  func tryLock() -> Bool
  @discardableResult
  func lock(before limit: NSDate) -> Bool
  @available(watchOS 2.0, *)
  var name: String?
}
@available(watchOS 2.0, *)
class NSCondition : NSObject, NSLocking {
  func wait()
  @discardableResult
  func wait(until limit: NSDate) -> Bool
  func signal()
  func broadcast()
  @available(watchOS 2.0, *)
  var name: String?
}
