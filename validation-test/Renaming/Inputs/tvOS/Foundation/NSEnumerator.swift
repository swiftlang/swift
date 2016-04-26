
struct NSFastEnumerationState {
  var state: UInt
  var itemsPtr: AutoreleasingUnsafeMutablePointer<AnyObject?>?
  var mutationsPtr: UnsafeMutablePointer<UInt>?
  var extra: (UInt, UInt, UInt, UInt, UInt)
  init()
  init(state state: UInt, itemsPtr itemsPtr: AutoreleasingUnsafeMutablePointer<AnyObject?>?, mutationsPtr mutationsPtr: UnsafeMutablePointer<UInt>?, extra extra: (UInt, UInt, UInt, UInt, UInt))
}
protocol NSFastEnumeration {
  @discardableResult
  func countByEnumerating(with state: UnsafeMutablePointer<NSFastEnumerationState>, objects buffer: AutoreleasingUnsafeMutablePointer<AnyObject>!, count len: Int) -> Int
}
class NSEnumerator : NSObject, NSFastEnumeration {
  @discardableResult
  func nextObject() -> AnyObject?
}

extension NSEnumerator : Sequence {
}
extension NSEnumerator {
  var allObjects: [AnyObject] { get }
}
