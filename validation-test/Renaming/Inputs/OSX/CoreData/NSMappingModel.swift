
@available(OSX 10.5, *)
class NSMappingModel : NSObject {
  /*not inherited*/ init?(from bundles: [NSBundle]?, forSourceModel sourceModel: NSManagedObjectModel?, destinationModel destinationModel: NSManagedObjectModel?)
  @available(OSX 10.6, *)
  @discardableResult
  class func inferredMappingModel(forSourceModel sourceModel: NSManagedObjectModel, destinationModel destinationModel: NSManagedObjectModel) throws -> NSMappingModel
  init?(contentsOf url: NSURL?)
  var entityMappings: [NSEntityMapping]!
  var entityMappingsByName: [String : NSEntityMapping] { get }
}
struct __modelMappingFlags {
  var _isInUse: UInt32
  var _reservedModelMapping: UInt32
  init()
  init(_isInUse _isInUse: UInt32, _reservedModelMapping _reservedModelMapping: UInt32)
}
