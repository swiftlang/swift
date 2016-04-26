
enum CFURLPathStyle : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case cfurlposixPathStyle
  @available(watchOS, introduced: 2.0, deprecated: 2.0)
  case cfurlhfsPathStyle
  case cfurlWindowsPathStyle
}
class CFURL {
}
@discardableResult
func CFURLGetTypeID() -> CFTypeID
@discardableResult
func CFURLCreateWithBytes(_ allocator: CFAllocator!, _ URLBytes: UnsafePointer<UInt8>!, _ length: CFIndex, _ encoding: CFStringEncoding, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateData(_ allocator: CFAllocator!, _ url: CFURL!, _ encoding: CFStringEncoding, _ escapeWhitespace: Bool) -> CFData!
@discardableResult
func CFURLCreateWithString(_ allocator: CFAllocator!, _ URLString: CFString!, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateAbsoluteURLWithBytes(_ alloc: CFAllocator!, _ relativeURLBytes: UnsafePointer<UInt8>!, _ length: CFIndex, _ encoding: CFStringEncoding, _ baseURL: CFURL!, _ useCompatibilityMode: Bool) -> CFURL!
@discardableResult
func CFURLCreateWithFileSystemPath(_ allocator: CFAllocator!, _ filePath: CFString!, _ pathStyle: CFURLPathStyle, _ isDirectory: Bool) -> CFURL!
@discardableResult
func CFURLCreateFromFileSystemRepresentation(_ allocator: CFAllocator!, _ buffer: UnsafePointer<UInt8>!, _ bufLen: CFIndex, _ isDirectory: Bool) -> CFURL!
@discardableResult
func CFURLCreateWithFileSystemPathRelativeToBase(_ allocator: CFAllocator!, _ filePath: CFString!, _ pathStyle: CFURLPathStyle, _ isDirectory: Bool, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateFromFileSystemRepresentationRelativeToBase(_ allocator: CFAllocator!, _ buffer: UnsafePointer<UInt8>!, _ bufLen: CFIndex, _ isDirectory: Bool, _ baseURL: CFURL!) -> CFURL!
@discardableResult
func CFURLGetFileSystemRepresentation(_ url: CFURL!, _ resolveAgainstBase: Bool, _ buffer: UnsafeMutablePointer<UInt8>!, _ maxBufLen: CFIndex) -> Bool
@discardableResult
func CFURLCopyAbsoluteURL(_ relativeURL: CFURL!) -> CFURL!
@discardableResult
func CFURLGetString(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLGetBaseURL(_ anURL: CFURL!) -> CFURL!
@discardableResult
func CFURLCanBeDecomposed(_ anURL: CFURL!) -> Bool
@discardableResult
func CFURLCopyScheme(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyNetLocation(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyPath(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyStrictPath(_ anURL: CFURL!, _ isAbsolute: UnsafeMutablePointer<DarwinBoolean>!) -> CFString!
@discardableResult
func CFURLCopyFileSystemPath(_ anURL: CFURL!, _ pathStyle: CFURLPathStyle) -> CFString!
@discardableResult
func CFURLHasDirectoryPath(_ anURL: CFURL!) -> Bool
@discardableResult
func CFURLCopyResourceSpecifier(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyHostName(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLGetPortNumber(_ anURL: CFURL!) -> Int32
@discardableResult
func CFURLCopyUserName(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyPassword(_ anURL: CFURL!) -> CFString!
@discardableResult
func CFURLCopyParameterString(_ anURL: CFURL!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@discardableResult
func CFURLCopyQueryString(_ anURL: CFURL!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@discardableResult
func CFURLCopyFragment(_ anURL: CFURL!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@discardableResult
func CFURLCopyLastPathComponent(_ url: CFURL!) -> CFString!
@discardableResult
func CFURLCopyPathExtension(_ url: CFURL!) -> CFString!
@discardableResult
func CFURLCreateCopyAppendingPathComponent(_ allocator: CFAllocator!, _ url: CFURL!, _ pathComponent: CFString!, _ isDirectory: Bool) -> CFURL!
@discardableResult
func CFURLCreateCopyDeletingLastPathComponent(_ allocator: CFAllocator!, _ url: CFURL!) -> CFURL!
@discardableResult
func CFURLCreateCopyAppendingPathExtension(_ allocator: CFAllocator!, _ url: CFURL!, _ extension: CFString!) -> CFURL!
@discardableResult
func CFURLCreateCopyDeletingPathExtension(_ allocator: CFAllocator!, _ url: CFURL!) -> CFURL!
@discardableResult
func CFURLGetBytes(_ url: CFURL!, _ buffer: UnsafeMutablePointer<UInt8>!, _ bufferLength: CFIndex) -> CFIndex
enum CFURLComponentType : CFIndex {
  init?(rawValue rawValue: CFIndex)
  var rawValue: CFIndex { get }
  case scheme
  case netLocation
  case path
  case resourceSpecifier
  case user
  case password
  case userInfo
  case host
  case port
  case parameterString
  case query
  case fragment
}
@discardableResult
func CFURLGetByteRangeForComponent(_ url: CFURL!, _ component: CFURLComponentType, _ rangeIncludingSeparators: UnsafeMutablePointer<CFRange>!) -> CFRange
@discardableResult
func CFURLCreateStringByReplacingPercentEscapes(_ allocator: CFAllocator!, _ originalString: CFString!, _ charactersToLeaveEscaped: CFString!) -> CFString!
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use [NSString stringByRemovingPercentEncoding] or CFURLCreateStringByReplacingPercentEscapes() instead, which always uses the recommended UTF-8 encoding.")
@discardableResult
func CFURLCreateStringByReplacingPercentEscapesUsingEncoding(_ allocator: CFAllocator!, _ origString: CFString!, _ charsToLeaveEscaped: CFString!, _ encoding: CFStringEncoding) -> CFString!
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use [NSString stringByAddingPercentEncodingWithAllowedCharacters:] instead, which always uses the recommended UTF-8 encoding, and which encodes for a specific URL component or subcomponent (since each URL component or subcomponent has different rules for what characters are valid).")
@discardableResult
func CFURLCreateStringByAddingPercentEscapes(_ allocator: CFAllocator!, _ originalString: CFString!, _ charactersToLeaveUnescaped: CFString!, _ legalURLCharactersToBeEscaped: CFString!, _ encoding: CFStringEncoding) -> CFString!
@available(watchOS 2.0, *)
@discardableResult
func CFURLIsFileReferenceURL(_ url: CFURL!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFURLCreateFileReferenceURL(_ allocator: CFAllocator!, _ url: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFURL>!
@available(watchOS 2.0, *)
@discardableResult
func CFURLCreateFilePathURL(_ allocator: CFAllocator!, _ url: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFURL>!
@available(watchOS, introduced: 2.0, deprecated: 2.0)
@discardableResult
func CFURLCreateFromFSRef(_ allocator: CFAllocator!, _ fsRef: OpaquePointer!) -> CFURL!
@available(watchOS, introduced: 2.0, deprecated: 2.0)
@discardableResult
func CFURLGetFSRef(_ url: CFURL!, _ fsRef: OpaquePointer!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFURLCopyResourcePropertyForKey(_ url: CFURL!, _ key: CFString!, _ propertyValueTypeRefPtr: UnsafeMutablePointer<Void>!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFURLCopyResourcePropertiesForKeys(_ url: CFURL!, _ keys: CFArray!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFDictionary>!
@available(watchOS 2.0, *)
@discardableResult
func CFURLSetResourcePropertyForKey(_ url: CFURL!, _ key: CFString!, _ propertyValue: CFTypeRef!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFURLSetResourcePropertiesForKeys(_ url: CFURL!, _ keyedPropertyValues: CFDictionary!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(watchOS 2.0, *)
let kCFURLKeysOfUnsetValuesKey: CFString!
@available(watchOS 2.0, *)
func CFURLClearResourcePropertyCacheForKey(_ url: CFURL!, _ key: CFString!)
@available(watchOS 2.0, *)
func CFURLClearResourcePropertyCache(_ url: CFURL!)
@available(watchOS 2.0, *)
func CFURLSetTemporaryResourcePropertyForKey(_ url: CFURL!, _ key: CFString!, _ propertyValue: CFTypeRef!)
@available(watchOS 2.0, *)
@discardableResult
func CFURLResourceIsReachable(_ url: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(watchOS 2.0, *)
let kCFURLNameKey: CFString!
@available(watchOS 2.0, *)
let kCFURLLocalizedNameKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsRegularFileKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsDirectoryKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsSymbolicLinkKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsVolumeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsPackageKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsApplicationKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsSystemImmutableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsUserImmutableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsHiddenKey: CFString!
@available(watchOS 2.0, *)
let kCFURLHasHiddenExtensionKey: CFString!
@available(watchOS 2.0, *)
let kCFURLCreationDateKey: CFString!
@available(watchOS 2.0, *)
let kCFURLContentAccessDateKey: CFString!
@available(watchOS 2.0, *)
let kCFURLContentModificationDateKey: CFString!
@available(watchOS 2.0, *)
let kCFURLAttributeModificationDateKey: CFString!
@available(watchOS 2.0, *)
let kCFURLLinkCountKey: CFString!
@available(watchOS 2.0, *)
let kCFURLParentDirectoryURLKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeURLKey: CFString!
@available(watchOS 2.0, *)
let kCFURLTypeIdentifierKey: CFString!
@available(watchOS 2.0, *)
let kCFURLLocalizedTypeDescriptionKey: CFString!
@available(watchOS 2.0, *)
let kCFURLLabelNumberKey: CFString!
@available(watchOS 2.0, *)
let kCFURLLabelColorKey: CFString!
@available(watchOS 2.0, *)
let kCFURLLocalizedLabelKey: CFString!
@available(watchOS 2.0, *)
let kCFURLEffectiveIconKey: CFString!
@available(watchOS 2.0, *)
let kCFURLCustomIconKey: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceIdentifierKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIdentifierKey: CFString!
@available(watchOS 2.0, *)
let kCFURLPreferredIOBlockSizeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsReadableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsWritableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsExecutableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLFileSecurityKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsExcludedFromBackupKey: CFString!
@available(watchOS 2.0, *)
let kCFURLPathKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsMountTriggerKey: CFString!
@available(watchOS 2.0, *)
let kCFURLGenerationIdentifierKey: CFString!
@available(watchOS 2.0, *)
let kCFURLDocumentIdentifierKey: CFString!
@available(watchOS 2.0, *)
let kCFURLAddedToDirectoryDateKey: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeNamedPipe: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeCharacterSpecial: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeDirectory: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeBlockSpecial: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeRegular: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeSymbolicLink: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeSocket: CFString!
@available(watchOS 2.0, *)
let kCFURLFileResourceTypeUnknown: CFString!
@available(watchOS 2.0, *)
let kCFURLFileSizeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLFileAllocatedSizeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLTotalFileSizeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLTotalFileAllocatedSizeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsAliasFileKey: CFString!
@available(watchOS 2.0, *)
let kCFURLFileProtectionKey: CFString!
@available(watchOS 2.0, *)
let kCFURLFileProtectionNone: CFString!
@available(watchOS 2.0, *)
let kCFURLFileProtectionComplete: CFString!
@available(watchOS 2.0, *)
let kCFURLFileProtectionCompleteUnlessOpen: CFString!
@available(watchOS 2.0, *)
let kCFURLFileProtectionCompleteUntilFirstUserAuthentication: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeLocalizedFormatDescriptionKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeTotalCapacityKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeAvailableCapacityKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeResourceCountKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsPersistentIDsKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsSymbolicLinksKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsHardLinksKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsJournalingKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsJournalingKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsSparseFilesKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsZeroRunsKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsCaseSensitiveNamesKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsCasePreservedNamesKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsRootDirectoryDatesKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsVolumeSizesKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsRenamingKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsAdvisoryFileLockingKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeSupportsExtendedSecurityKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsBrowsableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeMaximumFileSizeKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsEjectableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsRemovableKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsInternalKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsAutomountedKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsLocalKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeIsReadOnlyKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeCreationDateKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeURLForRemountingKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeUUIDStringKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeNameKey: CFString!
@available(watchOS 2.0, *)
let kCFURLVolumeLocalizedNameKey: CFString!
@available(watchOS 2.0, *)
let kCFURLIsUbiquitousItemKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemHasUnresolvedConflictsKey: CFString!
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use kCFURLUbiquitousItemDownloadingStatusKey instead")
let kCFURLUbiquitousItemIsDownloadedKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemIsDownloadingKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemIsUploadedKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemIsUploadingKey: CFString!
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSMetadataQuery and NSMetadataUbiquitousItemPercentDownloadedKey on NSMetadataItem instead")
let kCFURLUbiquitousItemPercentDownloadedKey: CFString!
@available(watchOS, introduced: 2.0, deprecated: 2.0, message: "Use NSMetadataQuery and NSMetadataUbiquitousItemPercentUploadedKey on NSMetadataItem instead")
let kCFURLUbiquitousItemPercentUploadedKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemDownloadingStatusKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemDownloadingErrorKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemUploadingErrorKey: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemDownloadingStatusNotDownloaded: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemDownloadingStatusDownloaded: CFString!
@available(watchOS 2.0, *)
let kCFURLUbiquitousItemDownloadingStatusCurrent: CFString!
@available(watchOS 2.0, *)
struct CFURLBookmarkCreationOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var minimalBookmarkMask: CFURLBookmarkCreationOptions { get }
  static var suitableForBookmarkFile: CFURLBookmarkCreationOptions { get }
  @available(watchOS, introduced: 2.0, deprecated: 2.0, message: "kCFURLBookmarkCreationPreferFileIDResolutionMask does nothing and has no effect on bookmark resolution")
  static var preferFileIDResolutionMask: CFURLBookmarkCreationOptions { get }
}
@available(watchOS 2.0, *)
struct CFURLBookmarkResolutionOptions : OptionSet {
  init(rawValue rawValue: CFOptionFlags)
  let rawValue: CFOptionFlags
  static var cfurlBookmarkResolutionWithoutUIMask: CFURLBookmarkResolutionOptions { get }
  static var cfurlBookmarkResolutionWithoutMountingMask: CFURLBookmarkResolutionOptions { get }
  static var cfBookmarkResolutionWithoutUIMask: CFURLBookmarkResolutionOptions { get }
  static var cfBookmarkResolutionWithoutMountingMask: CFURLBookmarkResolutionOptions { get }
}
typealias CFURLBookmarkFileCreationOptions = CFOptionFlags
@available(watchOS 2.0, *)
@discardableResult
func CFURLCreateBookmarkData(_ allocator: CFAllocator!, _ url: CFURL!, _ options: CFURLBookmarkCreationOptions, _ resourcePropertiesToInclude: CFArray!, _ relativeToURL: CFURL!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFData>!
@available(watchOS 2.0, *)
@discardableResult
func CFURLCreateByResolvingBookmarkData(_ allocator: CFAllocator!, _ bookmark: CFData!, _ options: CFURLBookmarkResolutionOptions, _ relativeToURL: CFURL!, _ resourcePropertiesToInclude: CFArray!, _ isStale: UnsafeMutablePointer<DarwinBoolean>!, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFURL>!
@available(watchOS 2.0, *)
@discardableResult
func CFURLCreateResourcePropertiesForKeysFromBookmarkData(_ allocator: CFAllocator!, _ resourcePropertiesToReturn: CFArray!, _ bookmark: CFData!) -> Unmanaged<CFDictionary>!
@available(watchOS 2.0, *)
@discardableResult
func CFURLCreateResourcePropertyForKeyFromBookmarkData(_ allocator: CFAllocator!, _ resourcePropertyKey: CFString!, _ bookmark: CFData!) -> Unmanaged<CFTypeRef>!
@available(watchOS 2.0, *)
@discardableResult
func CFURLCreateBookmarkDataFromFile(_ allocator: CFAllocator!, _ fileURL: CFURL!, _ errorRef: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Unmanaged<CFData>!
@available(watchOS 2.0, *)
@discardableResult
func CFURLWriteBookmarkDataToFile(_ bookmarkRef: CFData!, _ fileURL: CFURL!, _ options: CFURLBookmarkFileCreationOptions, _ errorRef: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(watchOS 2.0, *)
@discardableResult
func CFURLStartAccessingSecurityScopedResource(_ url: CFURL!) -> Bool
@available(watchOS 2.0, *)
func CFURLStopAccessingSecurityScopedResource(_ url: CFURL!)
