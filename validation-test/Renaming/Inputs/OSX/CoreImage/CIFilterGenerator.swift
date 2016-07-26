
let kCIFilterGeneratorExportedKey: String
let kCIFilterGeneratorExportedKeyTargetObject: String
let kCIFilterGeneratorExportedKeyName: String
@available(OSX 10.5, *)
class CIFilterGenerator : NSObject, NSSecureCoding, NSCopying, CIFilterConstructor {
  init?(contentsOf aURL: NSURL)
  func connect(_ sourceObject: AnyObject, withKey sourceKey: String?, to targetObject: AnyObject, withKey targetKey: String)
  func disconnectObject(_ sourceObject: AnyObject, withKey key: String, to targetObject: AnyObject, withKey targetKey: String)
  func exportKey(_ key: String, from targetObject: AnyObject, withName exportedKeyName: String?)
  func removeExportedKey(_ exportedKeyName: String)
  var exportedKeys: [NSObject : AnyObject] { get }
  func setAttributes(_ attributes: [NSObject : AnyObject], forExportedKey key: String)
  var classAttributes: [NSObject : AnyObject]
  @discardableResult
  func filter() -> CIFilter
  func registerFilterName(_ name: String)
  @discardableResult
  func write(to aURL: NSURL, atomically flag: Bool) -> Bool
}
