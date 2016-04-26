
@available(OSX 10.10, *)
class CKQuery : NSObject, NSSecureCoding, NSCopying {
  init(recordType recordType: String, predicate predicate: NSPredicate)
  var recordType: String { get }
  @NSCopying var predicate: NSPredicate { get }
  var sortDescriptors: [NSSortDescriptor]?
}
