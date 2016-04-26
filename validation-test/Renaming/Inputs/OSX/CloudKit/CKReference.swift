
@available(OSX 10.10, *)
enum CKReferenceAction : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case none
  case deleteSelf
}
@available(OSX 10.10, *)
class CKReference : NSObject, NSSecureCoding, NSCopying {
  init(recordID recordID: CKRecordID, action action: CKReferenceAction)
  convenience init(record record: CKRecord, action action: CKReferenceAction)
  var referenceAction: CKReferenceAction { get }
  @NSCopying var recordID: CKRecordID { get }
}
