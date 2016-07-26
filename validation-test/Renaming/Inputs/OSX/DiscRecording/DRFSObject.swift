
typealias DRFilesystemInclusionMask = UInt32
var DRFilesystemInclusionMaskISO9660: Int { get }
var DRFilesystemInclusionMaskJoliet: Int { get }
var DRFilesystemInclusionMaskUDF: Int { get }
var DRFilesystemInclusionMaskHFSPlus: Int { get }
class DRFSObject : NSObject {
  @discardableResult
  func isVirtual() -> Bool
  @discardableResult
  func sourcePath() -> String!
  @discardableResult
  func parent() -> DRFolder!
  @discardableResult
  func baseName() -> String!
  func setBaseName(_ baseName: String!)
  @discardableResult
  func specificName(forFilesystem filesystem: String!) -> String!
  @discardableResult
  func specificNames() -> [NSObject : AnyObject]!
  func setSpecificName(_ name: String!, forFilesystem filesystem: String!)
  func setSpecificNames(_ specificNames: [NSObject : AnyObject]!)
  @discardableResult
  func mangledName(forFilesystem filesystem: String!) -> String!
  @discardableResult
  func mangledNames() -> [NSObject : AnyObject]!
  @discardableResult
  func property(forKey key: String!, inFilesystem filesystem: String!, mergeWithOtherFilesystems merge: Bool) -> AnyObject!
  @discardableResult
  func properties(forFilesystem filesystem: String!, mergeWithOtherFilesystems merge: Bool) -> [NSObject : AnyObject]!
  func setProperty(_ property: AnyObject!, forKey key: String!, inFilesystem filesystem: String!)
  func setProperties(_ properties: [NSObject : AnyObject]!, inFilesystem filesystem: String!)
  @discardableResult
  func explicitFilesystemMask() -> DRFilesystemInclusionMask
  func setExplicitFilesystemMask(_ mask: DRFilesystemInclusionMask)
  @discardableResult
  func effectiveFilesystemMask() -> DRFilesystemInclusionMask
}
@available(OSX 10.2, *)
let DRAllFilesystems: String
@available(OSX 10.2, *)
let DRISO9660: String
@available(OSX 10.2, *)
let DRISO9660LevelOne: String
@available(OSX 10.2, *)
let DRISO9660LevelTwo: String
@available(OSX 10.2, *)
let DRJoliet: String
@available(OSX 10.2, *)
let DRHFSPlus: String
@available(OSX 10.4, *)
let DRUDF: String
@available(OSX 10.2, *)
let DRISO9660VersionNumber: String
@available(OSX 10.2, *)
let DRInvisible: String
@available(OSX 10.2, *)
let DRCreationDate: String
@available(OSX 10.2, *)
let DRContentModificationDate: String
@available(OSX 10.2, *)
let DRAttributeModificationDate: String
@available(OSX 10.2, *)
let DRAccessDate: String
@available(OSX 10.2, *)
let DRBackupDate: String
@available(OSX 10.2, *)
let DREffectiveDate: String
@available(OSX 10.2, *)
let DRExpirationDate: String
@available(OSX 10.2, *)
let DRRecordingDate: String
@available(OSX 10.2, *)
let DRPosixFileMode: String
@available(OSX 10.2, *)
let DRPosixUID: String
@available(OSX 10.2, *)
let DRPosixGID: String
@available(OSX 10.2, *)
let DRHFSPlusTextEncodingHint: String
@available(OSX 10.2, *)
let DRHFSPlusCatalogNodeID: String
@available(OSX 10.2, *)
let DRMacFileType: String
@available(OSX 10.2, *)
let DRMacFileCreator: String
@available(OSX 10.2, *)
let DRMacWindowBounds: String
@available(OSX 10.2, *)
let DRMacIconLocation: String
@available(OSX 10.2, *)
let DRMacScrollPosition: String
@available(OSX 10.2, *)
let DRMacWindowView: String
@available(OSX 10.2, *)
let DRMacFinderFlags: String
@available(OSX 10.2, *)
let DRMacExtendedFinderFlags: String
@available(OSX 10.5, *)
let DRMacFinderHideExtension: String
@available(OSX 10.4, *)
let DRUDFWriteVersion: String
@available(OSX 10.4, *)
let DRUDFVersion102: String
@available(OSX 10.4, *)
let DRUDFVersion150: String
@available(OSX 10.4, *)
let DRUDFPrimaryVolumeDescriptorNumber: String
@available(OSX 10.4, *)
let DRUDFVolumeSequenceNumber: String
@available(OSX 10.4, *)
let DRUDFMaxVolumeSequenceNumber: String
@available(OSX 10.4, *)
let DRUDFInterchangeLevel: String
@available(OSX 10.4, *)
let DRUDFMaxInterchangeLevel: String
@available(OSX 10.4, *)
let DRUDFApplicationIdentifierSuffix: String
@available(OSX 10.4, *)
let DRUDFVolumeSetIdentifier: String
@available(OSX 10.4, *)
let DRUDFVolumeSetTimestamp: String
@available(OSX 10.4, *)
let DRUDFVolumeSetImplementationUse: String
@available(OSX 10.4, *)
let DRUDFRealTimeFile: String
@available(OSX 10.4, *)
let DRUDFExtendedFilePermissions: String
