
enum NSTestComparisonOperation : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case equalToComparison
  case lessThanOrEqualToComparison
  case lessThanComparison
  case greaterThanOrEqualToComparison
  case greaterThanComparison
  case beginsWithComparison
  case endsWithComparison
  case containsComparison
}
class NSScriptWhoseTest : NSObject, NSCoding {
  @discardableResult
  func isTrue() -> Bool
}
class NSLogicalTest : NSScriptWhoseTest {
  init(andTestWith subTests: [NSSpecifierTest])
  init(orTestWith subTests: [NSSpecifierTest])
  init(notTestWith subTest: NSScriptWhoseTest)
}
class NSSpecifierTest : NSScriptWhoseTest {
  init(objectSpecifier obj1: NSScriptObjectSpecifier?, comparisonOperator compOp: NSTestComparisonOperation, test obj2: AnyObject?)
}
extension NSObject {
  @discardableResult
  class func isEqual(to object: AnyObject?) -> Bool
  @discardableResult
  func isEqual(to object: AnyObject?) -> Bool
  @discardableResult
  class func isLessThanOrEqual(to object: AnyObject?) -> Bool
  @discardableResult
  func isLessThanOrEqual(to object: AnyObject?) -> Bool
  @discardableResult
  class func isLessThan(_ object: AnyObject?) -> Bool
  @discardableResult
  func isLessThan(_ object: AnyObject?) -> Bool
  @discardableResult
  class func isGreaterThanOrEqual(to object: AnyObject?) -> Bool
  @discardableResult
  func isGreaterThanOrEqual(to object: AnyObject?) -> Bool
  @discardableResult
  class func isGreaterThan(_ object: AnyObject?) -> Bool
  @discardableResult
  func isGreaterThan(_ object: AnyObject?) -> Bool
  @discardableResult
  class func isNotEqual(to object: AnyObject?) -> Bool
  @discardableResult
  func isNotEqual(to object: AnyObject?) -> Bool
  @discardableResult
  class func doesContain(_ object: AnyObject) -> Bool
  @discardableResult
  func doesContain(_ object: AnyObject) -> Bool
  @discardableResult
  class func isLike(_ object: String) -> Bool
  @discardableResult
  func isLike(_ object: String) -> Bool
  @discardableResult
  class func isCaseInsensitiveLike(_ object: String) -> Bool
  @discardableResult
  func isCaseInsensitiveLike(_ object: String) -> Bool
}
extension NSObject {
  @discardableResult
  class func scriptingIsEqual(to object: AnyObject) -> Bool
  @discardableResult
  func scriptingIsEqual(to object: AnyObject) -> Bool
  @discardableResult
  class func scriptingIsLessThanOrEqual(to object: AnyObject) -> Bool
  @discardableResult
  func scriptingIsLessThanOrEqual(to object: AnyObject) -> Bool
  @discardableResult
  class func scriptingIsLessThan(_ object: AnyObject) -> Bool
  @discardableResult
  func scriptingIsLessThan(_ object: AnyObject) -> Bool
  @discardableResult
  class func scriptingIsGreaterThanOrEqual(to object: AnyObject) -> Bool
  @discardableResult
  func scriptingIsGreaterThanOrEqual(to object: AnyObject) -> Bool
  @discardableResult
  class func scriptingIsGreaterThan(_ object: AnyObject) -> Bool
  @discardableResult
  func scriptingIsGreaterThan(_ object: AnyObject) -> Bool
  @discardableResult
  class func scriptingBegins(with object: AnyObject) -> Bool
  @discardableResult
  func scriptingBegins(with object: AnyObject) -> Bool
  @discardableResult
  class func scriptingEnds(with object: AnyObject) -> Bool
  @discardableResult
  func scriptingEnds(with object: AnyObject) -> Bool
  @discardableResult
  class func scriptingContains(_ object: AnyObject) -> Bool
  @discardableResult
  func scriptingContains(_ object: AnyObject) -> Bool
}
