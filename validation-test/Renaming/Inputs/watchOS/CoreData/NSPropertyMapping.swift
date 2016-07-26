
@available(watchOS 2.0, *)
class NSPropertyMapping : NSObject {
  var name: String?
  var valueExpression: NSExpression?
  var userInfo: [NSObject : AnyObject]?
}
struct __propertyMappingFlags {
  var _isInUse: UInt32
  var _reservedPropertyMapping: UInt32
  init()
  init(_isInUse _isInUse: UInt32, _reservedPropertyMapping _reservedPropertyMapping: UInt32)
}
