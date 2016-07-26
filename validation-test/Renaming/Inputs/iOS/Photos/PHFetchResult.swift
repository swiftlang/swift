
@available(iOS 8.0, *)
class PHFetchResult<ObjectType : AnyObject> : NSObject, NSCopying, NSFastEnumeration {
  var count: Int { get }
  @discardableResult
  func object(at index: Int) -> ObjectType
  subscript(_ idx: Int) -> ObjectType { get }
  @discardableResult
  func contains(_ anObject: ObjectType) -> Bool
  @discardableResult
  func index(of anObject: ObjectType) -> Int
  @discardableResult
  func index(of anObject: ObjectType, in range: NSRange) -> Int
  var firstObject: ObjectType? { get }
  var lastObject: ObjectType? { get }
  @discardableResult
  func objects(at indexes: NSIndexSet) -> [ObjectType]
  func enumerateObjects(_ block: (ObjectType, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  func enumerateObjects(_ opts: NSEnumerationOptions = [], using block: (ObjectType, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  func enumerateObjects(at s: NSIndexSet, options opts: NSEnumerationOptions = [], using block: (ObjectType, Int, UnsafeMutablePointer<ObjCBool>) -> Void)
  @discardableResult
  func countOfAssets(with mediaType: PHAssetMediaType) -> Int
  init()
}
