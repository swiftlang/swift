
@available(watchOS 2.0, *)
class CNGroup : NSObject, NSCopying, NSMutableCopying, NSSecureCoding {
  var identifier: String { get }
  var name: String { get }
}
@available(watchOS 2.0, *)
let CNGroupIdentifierKey: String
@available(watchOS 2.0, *)
let CNGroupNameKey: String
