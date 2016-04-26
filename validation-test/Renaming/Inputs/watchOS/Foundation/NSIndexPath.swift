
class NSIndexPath : NSObject, NSCopying, NSSecureCoding {
  init(indexes indexes: UnsafePointer<Int>!, length length: Int)
  convenience init(index index: Int)
  @discardableResult
  func adding(_ index: Int) -> NSIndexPath
  @discardableResult
  func removingLastIndex() -> NSIndexPath
  @discardableResult
  func index(atPosition position: Int) -> Int
  var length: Int { get }
  @available(watchOS 2.0, *)
  func getIndexes(_ indexes: UnsafeMutablePointer<Int>, range positionRange: NSRange)
  @discardableResult
  func compare(_ otherObject: NSIndexPath) -> NSComparisonResult
}
extension NSIndexPath {
  func getIndexes(_ indexes: UnsafeMutablePointer<Int>)
}
