
class EKObject : NSObject {
  var hasChanges: Bool { get }
  var isNew: Bool { get }
  func reset()
  func rollback()
  @discardableResult
  func refresh() -> Bool
}
