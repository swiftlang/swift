
class NSIndexSet : NSObject, NSCopying, NSMutableCopying, NSSecureCoding {
  init(indexesIn range: NSRange)
  init(indexSet indexSet: NSIndexSet)
  convenience init(index value: Int)
  @discardableResult
  func isEqual(to indexSet: NSIndexSet) -> Bool
  var count: Int { get }
  var firstIndex: Int { get }
  var lastIndex: Int { get }
  @discardableResult
  func indexGreaterThanIndex(_ value: Int) -> Int
  @discardableResult
  func indexLessThanIndex(_ value: Int) -> Int
  @discardableResult
  func indexGreaterThanOrEqual(to value: Int) -> Int
  @discardableResult
  func indexLessThanOrEqual(to value: Int) -> Int
  @discardableResult
  func getIndexes(_ indexBuffer: UnsafeMutablePointer<Int>, maxCount bufferSize: Int, inIndexRange range: NSRangePointer?) -> Int
  @available(tvOS 2.0, *)
  @discardableResult
  func countOfIndexes(in range: NSRange) -> Int
  @discardableResult
  func contains(_ value: Int) -> Bool
  @discardableResult
  func contains(in range: NSRange) -> Bool
  @discardableResult
  func contains(_ indexSet: NSIndexSet) -> Bool
  @discardableResult
  func intersects(in range: NSRange) -> Bool
  @available(tvOS 4.0, *)
  func enumerate(_ block: (Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 4.0, *)
  func enumerate(_ opts: NSEnumerationOptions = [], using block: (Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 4.0, *)
  func enumerate(in range: NSRange, options opts: NSEnumerationOptions = [], using block: (Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 4.0, *)
  @discardableResult
  func index(passingTest predicate: (Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Int
  @available(tvOS 4.0, *)
  @discardableResult
  func index(_ opts: NSEnumerationOptions = [], passingTest predicate: (Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Int
  @available(tvOS 4.0, *)
  @discardableResult
  func index(in range: NSRange, options opts: NSEnumerationOptions = [], passingTest predicate: (Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> Int
  @available(tvOS 4.0, *)
  @discardableResult
  func indexes(passingTest predicate: (Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> NSIndexSet
  @available(tvOS 4.0, *)
  @discardableResult
  func indexes(_ opts: NSEnumerationOptions = [], passingTest predicate: (Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> NSIndexSet
  @available(tvOS 4.0, *)
  @discardableResult
  func indexes(in range: NSRange, options opts: NSEnumerationOptions = [], passingTest predicate: (Int, UnsafeMutablePointer<ObjCBool>) -> Bool) -> NSIndexSet
  @available(tvOS 5.0, *)
  func enumerateRanges(_ block: (NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 5.0, *)
  func enumerateRanges(_ opts: NSEnumerationOptions = [], using block: (NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
  @available(tvOS 5.0, *)
  func enumerateRanges(in range: NSRange, options opts: NSEnumerationOptions = [], using block: (NSRange, UnsafeMutablePointer<ObjCBool>) -> Void)
}

extension NSIndexSet : Sequence {
}
class NSMutableIndexSet : NSIndexSet {
  func add(_ indexSet: NSIndexSet)
  func remove(_ indexSet: NSIndexSet)
  func removeAllIndexes()
  func add(_ value: Int)
  func remove(_ value: Int)
  func add(in range: NSRange)
  func remove(in range: NSRange)
  func shiftIndexesStarting(at index: Int, by delta: Int)
}
