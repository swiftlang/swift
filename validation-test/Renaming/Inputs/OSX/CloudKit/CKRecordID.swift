
@available(OSX 10.10, *)
class CKRecordID : NSObject, NSSecureCoding, NSCopying {
  convenience init(recordName recordName: String)
  init(recordName recordName: String, zoneID zoneID: CKRecordZoneID)
  var recordName: String { get }
  var zoneID: CKRecordZoneID { get }
}
