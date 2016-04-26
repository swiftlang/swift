
var __FOUNDATION_NSPOINTERFUNCTIONS__: Int32 { get }
struct NSPointerFunctionsOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  @available(OSX 10.5, *)
  static var strongMemory: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var opaqueMemory: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var mallocMemory: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var machVirtualMemory: NSPointerFunctionsOptions { get }
  @available(OSX 10.8, *)
  static var weakMemory: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var objectPersonality: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var opaquePersonality: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var objectPointerPersonality: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var cStringPersonality: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var structPersonality: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var integerPersonality: NSPointerFunctionsOptions { get }
  @available(OSX 10.5, *)
  static var copyIn: NSPointerFunctionsOptions { get }
}
@available(OSX 10.5, *)
class NSPointerFunctions : NSObject, NSCopying {
  init(options options: NSPointerFunctionsOptions = [])
  var hashFunction: (@convention(c) (UnsafePointer<Void>, (@convention(c) (UnsafePointer<Void>) -> Int)?) -> Int)?
  var isEqualFunction: (@convention(c) (UnsafePointer<Void>, UnsafePointer<Void>, (@convention(c) (UnsafePointer<Void>) -> Int)?) -> ObjCBool)?
  var sizeFunction: (@convention(c) (UnsafePointer<Void>) -> Int)?
  var descriptionFunction: (@convention(c) (UnsafePointer<Void>) -> String?)?
  var relinquishFunction: (@convention(c) (UnsafePointer<Void>, (@convention(c) (UnsafePointer<Void>) -> Int)?) -> Void)?
  var acquireFunction: (@convention(c) (UnsafePointer<Void>, (@convention(c) (UnsafePointer<Void>) -> Int)?, ObjCBool) -> UnsafeMutablePointer<Void>)?
  var usesStrongWriteBarrier: Bool
  var usesWeakReadAndWriteBarriers: Bool
}
