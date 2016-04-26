
class NSColorList : NSObject, NSSecureCoding {
  @discardableResult
  class func availableColorLists() -> [NSColorList]
  /*not inherited*/ init?(named name: String)
  init(name name: String)
  init?(name name: String, fromFile path: String?)
  var name: String? { get }
  func setColor(_ color: NSColor, forKey key: String)
  func insert(_ color: NSColor, key key: String, at loc: Int)
  func removeColor(withKey key: String)
  @discardableResult
  func color(withKey key: String) -> NSColor?
  var allKeys: [String] { get }
  var isEditable: Bool { get }
  @discardableResult
  func write(toFile path: String?) -> Bool
  func removeFile()
}
struct _colorListFlags {
  var colorsLoaded: UInt32
  var editable: UInt32
  var hasDeviceSpecificLists: UInt32
  var dirty: UInt32
  var hasFrozen: UInt32
  var notificationsDisabled: UInt32
  var hasAttemptedLoadingBundleForDirectory: UInt32
  var isProfileBased: UInt32
  init()
  init(colorsLoaded colorsLoaded: UInt32, editable editable: UInt32, hasDeviceSpecificLists hasDeviceSpecificLists: UInt32, dirty dirty: UInt32, hasFrozen hasFrozen: UInt32, notificationsDisabled notificationsDisabled: UInt32, hasAttemptedLoadingBundleForDirectory hasAttemptedLoadingBundleForDirectory: UInt32, isProfileBased isProfileBased: UInt32)
}
let NSColorListDidChangeNotification: String
