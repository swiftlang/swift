
@available(OSX 10.4, *)
class NSPropertyDescription : NSObject, NSCoding, NSCopying {
  unowned(unsafe) var entity: @sil_unmanaged NSEntityDescription { get }
  var name: String
  var isOptional: Bool
  var isTransient: Bool
  var validationPredicates: [NSPredicate] { get }
  var validationWarnings: [AnyObject] { get }
  func setValidationPredicates(_ validationPredicates: [NSPredicate]?, withValidationWarnings validationWarnings: [String]?)
  var userInfo: [NSObject : AnyObject]?
  @available(OSX 10.5, *)
  var isIndexed: Bool
  @available(OSX 10.5, *)
  @NSCopying var versionHash: NSData { get }
  @available(OSX 10.5, *)
  var versionHashModifier: String?
  @available(OSX 10.6, *)
  var isIndexedBySpotlight: Bool
  @available(OSX 10.6, *)
  var isStoredInExternalRecord: Bool
  @available(OSX 10.6, *)
  var renamingIdentifier: String?
}
struct __propertyDescriptionFlags {
  var _isReadOnly: UInt32
  var _isTransient: UInt32
  var _isOptional: UInt32
  var _isIndexed: UInt32
  var _skipValidation: UInt32
  var _isIndexedBySpotlight: UInt32
  var _isStoredInExternalRecord: UInt32
  var _extraIvarsAreInDataBlob: UInt32
  var _isOrdered: UInt32
  var _reservedPropertyDescription: UInt32
  init()
  init(_isReadOnly _isReadOnly: UInt32, _isTransient _isTransient: UInt32, _isOptional _isOptional: UInt32, _isIndexed _isIndexed: UInt32, _skipValidation _skipValidation: UInt32, _isIndexedBySpotlight _isIndexedBySpotlight: UInt32, _isStoredInExternalRecord _isStoredInExternalRecord: UInt32, _extraIvarsAreInDataBlob _extraIvarsAreInDataBlob: UInt32, _isOrdered _isOrdered: UInt32, _reservedPropertyDescription _reservedPropertyDescription: UInt32)
}
