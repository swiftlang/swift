
struct NSKeyValueObservingOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var new: NSKeyValueObservingOptions { get }
  static var old: NSKeyValueObservingOptions { get }
  @available(OSX 10.5, *)
  static var initial: NSKeyValueObservingOptions { get }
  @available(OSX 10.5, *)
  static var prior: NSKeyValueObservingOptions { get }
}
enum NSKeyValueChange : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case setting
  case insertion
  case removal
  case replacement
}
enum NSKeyValueSetMutationKind : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case unionSetMutation
  case minusSetMutation
  case intersectSetMutation
  case setSetMutation
}
let NSKeyValueChangeKindKey: String
let NSKeyValueChangeNewKey: String
let NSKeyValueChangeOldKey: String
let NSKeyValueChangeIndexesKey: String
@available(OSX 10.5, *)
let NSKeyValueChangeNotificationIsPriorKey: String
extension NSObject {
  class func observeValue(forKeyPath keyPath: String?, of object: AnyObject?, change change: [String : AnyObject]?, context context: UnsafeMutablePointer<Void>?)
  func observeValue(forKeyPath keyPath: String?, of object: AnyObject?, change change: [String : AnyObject]?, context context: UnsafeMutablePointer<Void>?)
}
extension NSObject {
  class func addObserver(_ observer: NSObject, forKeyPath keyPath: String, options options: NSKeyValueObservingOptions = [], context context: UnsafeMutablePointer<Void>?)
  func addObserver(_ observer: NSObject, forKeyPath keyPath: String, options options: NSKeyValueObservingOptions = [], context context: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  class func removeObserver(_ observer: NSObject, forKeyPath keyPath: String, context context: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  func removeObserver(_ observer: NSObject, forKeyPath keyPath: String, context context: UnsafeMutablePointer<Void>?)
  class func removeObserver(_ observer: NSObject, forKeyPath keyPath: String)
  func removeObserver(_ observer: NSObject, forKeyPath keyPath: String)
}
extension NSArray {
  func addObserver(_ observer: NSObject, toObjectsAt indexes: NSIndexSet, forKeyPath keyPath: String, options options: NSKeyValueObservingOptions = [], context context: UnsafeMutablePointer<Void>?)
  @available(OSX 10.7, *)
  func removeObserver(_ observer: NSObject, fromObjectsAt indexes: NSIndexSet, forKeyPath keyPath: String, context context: UnsafeMutablePointer<Void>?)
  func removeObserver(_ observer: NSObject, fromObjectsAt indexes: NSIndexSet, forKeyPath keyPath: String)
}
extension NSOrderedSet {
}
extension NSSet {
}
extension NSObject {
  class func willChangeValue(forKey key: String)
  func willChangeValue(forKey key: String)
  class func didChangeValue(forKey key: String)
  func didChangeValue(forKey key: String)
  class func willChange(_ changeKind: NSKeyValueChange, valuesAt indexes: NSIndexSet, forKey key: String)
  func willChange(_ changeKind: NSKeyValueChange, valuesAt indexes: NSIndexSet, forKey key: String)
  class func didChange(_ changeKind: NSKeyValueChange, valuesAt indexes: NSIndexSet, forKey key: String)
  func didChange(_ changeKind: NSKeyValueChange, valuesAt indexes: NSIndexSet, forKey key: String)
  class func willChangeValue(forKey key: String, withSetMutation mutationKind: NSKeyValueSetMutationKind, using objects: Set<NSObject>)
  func willChangeValue(forKey key: String, withSetMutation mutationKind: NSKeyValueSetMutationKind, using objects: Set<NSObject>)
  class func didChangeValue(forKey key: String, withSetMutation mutationKind: NSKeyValueSetMutationKind, using objects: Set<NSObject>)
  func didChangeValue(forKey key: String, withSetMutation mutationKind: NSKeyValueSetMutationKind, using objects: Set<NSObject>)
}
extension NSObject {
  @available(OSX 10.5, *)
  @discardableResult
  class func keyPathsForValuesAffectingValue(forKey key: String) -> Set<String>
  @discardableResult
  class func automaticallyNotifiesObservers(forKey key: String) -> Bool
  var observationInfo: UnsafeMutablePointer<Void>?
  class func observationInfo() -> UnsafeMutablePointer<Void>?
  class func setObservationInfo(_ observationInfo: UnsafeMutablePointer<Void>?)
}
extension NSObject {
}
