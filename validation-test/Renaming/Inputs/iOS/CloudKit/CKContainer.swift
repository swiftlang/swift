
@available(iOS 8.0, *)
let CKOwnerDefaultName: String
@available(iOS 8.0, *)
class CKContainer : NSObject {
  @discardableResult
  class func defaultContainer() -> CKContainer
  /*not inherited*/ init(identifier containerIdentifier: String)
  var containerIdentifier: String? { get }
  func add(_ operation: CKOperation)
}
extension CKContainer {
  var privateCloudDatabase: CKDatabase { get }
  var publicCloudDatabase: CKDatabase { get }
}
@available(iOS 8.0, *)
enum CKAccountStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case couldNotDetermine
  case available
  case restricted
  case noAccount
}
@available(iOS 9.0, *)
let CKAccountChangedNotification: String
extension CKContainer {
  func accountStatus(completionHandler completionHandler: (CKAccountStatus, NSError?) -> Void)
}
@available(iOS 8.0, *)
struct CKApplicationPermissions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var userDiscoverability: CKApplicationPermissions { get }
}
@available(iOS 8.0, *)
enum CKApplicationPermissionStatus : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case initialState
  case couldNotComplete
  case denied
  case granted
}
typealias CKApplicationPermissionBlock = (CKApplicationPermissionStatus, NSError?) -> Void
extension CKContainer {
  func status(forApplicationPermission applicationPermission: CKApplicationPermissions, completionHandler completionHandler: CKApplicationPermissionBlock)
  func requestApplicationPermission(_ applicationPermission: CKApplicationPermissions, completionHandler completionHandler: CKApplicationPermissionBlock)
}
extension CKContainer {
  func fetchUserRecordID(completionHandler completionHandler: (CKRecordID?, NSError?) -> Void)
  func discoverAllContactUserInfos(completionHandler completionHandler: ([CKDiscoveredUserInfo]?, NSError?) -> Void)
  func discoverUserInfo(withEmailAddress email: String, completionHandler completionHandler: (CKDiscoveredUserInfo?, NSError?) -> Void)
  func discoverUserInfo(withUserRecordID userRecordID: CKRecordID, completionHandler completionHandler: (CKDiscoveredUserInfo?, NSError?) -> Void)
}
extension CKContainer {
  @available(iOS 9.3, *)
  func fetchAllLongLivedOperationIDs(completionHandler completionHandler: ([String]?, NSError?) -> Void)
  @available(iOS 9.3, *)
  func fetchLongLivedOperation(withID operationID: String, completionHandler completionHandler: (CKOperation?, NSError?) -> Void)
}
