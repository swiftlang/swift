
@available(OSX 10.5, *)
class NSPointerArray : NSObject, NSFastEnumeration, NSCopying, NSCoding {
  init(options options: NSPointerFunctionsOptions = [])
  init(pointerFunctions functions: NSPointerFunctions)
  @NSCopying var pointerFunctions: NSPointerFunctions { get }
  @discardableResult
  func pointer(at index: Int) -> UnsafeMutablePointer<Void>?
  func addPointer(_ pointer: UnsafeMutablePointer<Void>?)
  func removePointer(at index: Int)
  func insertPointer(_ item: UnsafeMutablePointer<Void>?, at index: Int)
  func replacePointer(at index: Int, withPointer item: UnsafeMutablePointer<Void>?)
  func compact()
  var count: Int
}
extension NSPointerArray {
  @available(OSX 10.8, *)
  @discardableResult
  class func strongObjects() -> NSPointerArray
  @available(OSX 10.8, *)
  @discardableResult
  class func weakObjects() -> NSPointerArray
  var allObjects: [AnyObject] { get }
}
