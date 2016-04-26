
@available(OSX 10.10, *)
let CKRecordTypeUserRecord: String
protocol CKRecordValue : NSObjectProtocol {
}
@available(OSX 10.10, *)
class CKRecord : NSObject, NSSecureCoding, NSCopying {
  init(recordType recordType: String)
  init(recordType recordType: String, recordID recordID: CKRecordID)
  init(recordType recordType: String, zoneID zoneID: CKRecordZoneID)
  var recordType: String { get }
  @NSCopying var recordID: CKRecordID { get }
  var recordChangeTag: String? { get }
  @NSCopying var creatorUserRecordID: CKRecordID? { get }
  @NSCopying var creationDate: NSDate? { get }
  @NSCopying var lastModifiedUserRecordID: CKRecordID? { get }
  @NSCopying var modificationDate: NSDate? { get }
  @discardableResult
  func object(forKey key: String) -> CKRecordValue?
  func setObject(_ object: CKRecordValue?, forKey key: String)
  @discardableResult
  func allKeys() -> [String]
  @discardableResult
  func allTokens() -> [String]
  subscript(_ key: String) -> CKRecordValue?
  @discardableResult
  func changedKeys() -> [String]
  func encodeSystemFields(with coder: NSCoder)
}
extension NSString : CKRecordValue {
}
extension NSNumber : CKRecordValue {
}
extension NSArray : CKRecordValue {
}
extension NSDate : CKRecordValue {
}
extension NSData : CKRecordValue {
}
extension CKReference : CKRecordValue {
}
extension CKAsset : CKRecordValue {
}
extension CLLocation : CKRecordValue {
}
