
@available(iOS 8.0, *)
class HMHome : NSObject {
  weak var delegate: @sil_weak HMHomeDelegate?
  var name: String { get }
  var isPrimary: Bool { get }
  @available(iOS 9.0, *)
  @NSCopying var uniqueIdentifier: NSUUID { get }
  func updateName(_ name: String, completionHandler completion: (NSError?) -> Void)
}
extension HMHome {
  var accessories: [HMAccessory] { get }
  func addAccessory(_ accessory: HMAccessory, completionHandler completion: (NSError?) -> Void)
  func removeAccessory(_ accessory: HMAccessory, completionHandler completion: (NSError?) -> Void)
  func assignAccessory(_ accessory: HMAccessory, to room: HMRoom, completionHandler completion: (NSError?) -> Void)
  @discardableResult
  func servicesWithTypes(_ serviceTypes: [String]) -> [HMService]?
  func unblockAccessory(_ accessory: HMAccessory, completionHandler completion: (NSError?) -> Void)
}
extension HMHome {
  @available(iOS 9.0, *)
  var currentUser: HMUser { get }
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  var users: [HMUser] { get }
  @available(iOS 9.0, *)
  func manageUsers(completionHandler completion: (NSError?) -> Void)
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  func addUser(completionHandler completion: (HMUser?, NSError?) -> Void)
  @available(iOS, introduced: 8.0, deprecated: 9.0)
  func removeUser(_ user: HMUser, completionHandler completion: (NSError?) -> Void)
  @available(iOS 9.0, *)
  @discardableResult
  func homeAccessControl(for user: HMUser) -> HMHomeAccessControl
}
extension HMHome {
  var rooms: [HMRoom] { get }
  func removeRoom(_ room: HMRoom, completionHandler completion: (NSError?) -> Void)
  @discardableResult
  func roomForEntireHome() -> HMRoom
}
extension HMHome {
  var zones: [HMZone] { get }
  func removeZone(_ zone: HMZone, completionHandler completion: (NSError?) -> Void)
}
extension HMHome {
  var serviceGroups: [HMServiceGroup] { get }
  func removeServiceGroup(_ group: HMServiceGroup, completionHandler completion: (NSError?) -> Void)
}
extension HMHome {
  var actionSets: [HMActionSet] { get }
  func removeActionSet(_ actionSet: HMActionSet, completionHandler completion: (NSError?) -> Void)
  func executeActionSet(_ actionSet: HMActionSet, completionHandler completion: (NSError?) -> Void)
  @available(iOS 9.0, *)
  @discardableResult
  func builtinActionSet(ofType actionSetType: String) -> HMActionSet?
}
extension HMHome {
  var triggers: [HMTrigger] { get }
  func addTrigger(_ trigger: HMTrigger, completionHandler completion: (NSError?) -> Void)
  func removeTrigger(_ trigger: HMTrigger, completionHandler completion: (NSError?) -> Void)
}
@available(iOS 8.0, *)
protocol HMHomeDelegate : NSObjectProtocol {
  optional func homeDidUpdateName(_ home: HMHome)
  optional func home(_ home: HMHome, didAdd accessory: HMAccessory)
  optional func home(_ home: HMHome, didRemove accessory: HMAccessory)
  optional func home(_ home: HMHome, didAdd user: HMUser)
  optional func home(_ home: HMHome, didRemove user: HMUser)
  optional func home(_ home: HMHome, didUpdate room: HMRoom, for accessory: HMAccessory)
  optional func home(_ home: HMHome, didAdd room: HMRoom)
  optional func home(_ home: HMHome, didRemove room: HMRoom)
  optional func home(_ home: HMHome, didUpdateNameFor room: HMRoom)
  optional func home(_ home: HMHome, didAdd zone: HMZone)
  optional func home(_ home: HMHome, didRemove zone: HMZone)
  optional func home(_ home: HMHome, didUpdateNameFor zone: HMZone)
  optional func home(_ home: HMHome, didAdd room: HMRoom, to zone: HMZone)
  optional func home(_ home: HMHome, didRemove room: HMRoom, from zone: HMZone)
  optional func home(_ home: HMHome, didAdd group: HMServiceGroup)
  optional func home(_ home: HMHome, didRemove group: HMServiceGroup)
  optional func home(_ home: HMHome, didUpdateNameFor group: HMServiceGroup)
  optional func home(_ home: HMHome, didAdd service: HMService, to group: HMServiceGroup)
  optional func home(_ home: HMHome, didRemove service: HMService, from group: HMServiceGroup)
  optional func home(_ home: HMHome, didAdd actionSet: HMActionSet)
  optional func home(_ home: HMHome, didRemove actionSet: HMActionSet)
  optional func home(_ home: HMHome, didUpdateNameFor actionSet: HMActionSet)
  optional func home(_ home: HMHome, didUpdateActionsFor actionSet: HMActionSet)
  optional func home(_ home: HMHome, didAdd trigger: HMTrigger)
  optional func home(_ home: HMHome, didRemove trigger: HMTrigger)
  optional func home(_ home: HMHome, didUpdateNameFor trigger: HMTrigger)
  optional func home(_ home: HMHome, didUpdate trigger: HMTrigger)
  optional func home(_ home: HMHome, didUnblockAccessory accessory: HMAccessory)
  optional func home(_ home: HMHome, didEncounterError error: NSError, for accessory: HMAccessory)
}
@available(iOS 8.0, *)
let HMUserFailedAccessoriesKey: String
