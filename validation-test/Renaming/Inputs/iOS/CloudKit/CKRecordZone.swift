
@available(iOS 8.0, *)
struct CKRecordZoneCapabilities : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var fetchChanges: CKRecordZoneCapabilities { get }
  static var atomic: CKRecordZoneCapabilities { get }
}
@available(iOS 8.0, *)
let CKRecordZoneDefaultName: String
@available(iOS 8.0, *)
class CKRecordZone : NSObject, NSSecureCoding, NSCopying {
  @discardableResult
  class func defaultRecordZone() -> CKRecordZone
  init(zoneName zoneName: String)
  init(zoneID zoneID: CKRecordZoneID)
  var zoneID: CKRecordZoneID { get }
  var capabilities: CKRecordZoneCapabilities { get }
}
