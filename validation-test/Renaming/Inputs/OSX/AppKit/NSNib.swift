
class NSNib : NSObject, NSCoding {
  init?(nibNamed nibName: String, bundle bundle: NSBundle?)
  @available(OSX 10.8, *)
  init(nibData nibData: NSData, bundle bundle: NSBundle?)
  @available(OSX 10.8, *)
  @discardableResult
  func instantiate(withOwner owner: AnyObject?, topLevel topLevelObjects: AutoreleasingUnsafeMutablePointer<NSArray>?) -> Bool
}
struct _NSNibFlags {
  var _isKeyed: UInt32
  var _inheritsDecodeTimeBundle: UInt32
  var _inheritsDecodeTimePath: UInt32
  var _reserved: UInt32
  init()
  init(_isKeyed _isKeyed: UInt32, _inheritsDecodeTimeBundle _inheritsDecodeTimeBundle: UInt32, _inheritsDecodeTimePath _inheritsDecodeTimePath: UInt32, _reserved _reserved: UInt32)
}
extension NSNib {
}
