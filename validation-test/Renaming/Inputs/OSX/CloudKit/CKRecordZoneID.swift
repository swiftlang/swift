
@available(OSX 10.10, *)
class CKRecordZoneID : NSObject, NSSecureCoding, NSCopying {
  init(zoneName zoneName: String, ownerName ownerName: String)
  var zoneName: String { get }
  var ownerName: String { get }
}
