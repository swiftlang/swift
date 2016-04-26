
struct _NSRange {
  var location: Int
  var length: Int
  init()
  init(location location: Int, length length: Int)
}

extension NSRange {
  init(_ x: Range<Int>)
  @warn_unused_result
  func toRange() -> Range<Int>?
}

extension NSRange : CustomReflectable {
}

extension NSRange : CustomPlaygroundQuickLookable {
}

extension NSRange : _ObjectiveCBridgeable {
}
typealias NSRange = _NSRange
typealias NSRangePointer = UnsafeMutablePointer<NSRange>
@discardableResult
func NSMakeRange(_ loc: Int, _ len: Int) -> NSRange
@discardableResult
func NSMaxRange(_ range: NSRange) -> Int
@discardableResult
func NSLocationInRange(_ loc: Int, _ range: NSRange) -> Bool
@discardableResult
func NSEqualRanges(_ range1: NSRange, _ range2: NSRange) -> Bool
@discardableResult
func NSUnionRange(_ range1: NSRange, _ range2: NSRange) -> NSRange
@discardableResult
func NSIntersectionRange(_ range1: NSRange, _ range2: NSRange) -> NSRange
@discardableResult
func NSStringFromRange(_ range: NSRange) -> String
@discardableResult
func NSRangeFromString(_ aString: String) -> NSRange
extension NSValue {
  /*not inherited*/ init(range range: NSRange)
  var rangeValue: NSRange { get }
}
