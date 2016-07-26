
@available(watchOS 2.0, *)
class HKDeletedObject : NSObject, NSSecureCoding {
  var uuid: NSUUID { get }
}
