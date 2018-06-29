//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

/**
 URLs to file system resources support the properties defined below. Note that not all property values will exist for all file system URLs. For example, if a file is located on a volume that does not support creation dates, it is valid to request the creation date property, but the returned value will be nil, and no error will be generated.
 
 Only the fields requested by the keys you pass into the `URL` function to receive this value will be populated. The others will return `nil` regardless of the underlying property on the file system.
 
 As a convenience, volume resource values can be requested from any file system URL. The value returned will reflect the property value for the volume on which the resource is located.
*/
public struct URLResourceValues {
    fileprivate var _values: [URLResourceKey: Any]
    fileprivate var _keys: Set<URLResourceKey>
    
    public init() {
        _values = [:]
        _keys = []
    }
    
    fileprivate init(keys: Set<URLResourceKey>, values: [URLResourceKey: Any]) {
        _values = values
        _keys = keys
    }
    
    private func contains(_ key: URLResourceKey) -> Bool {
        return _keys.contains(key)
    }
    
    private func _get<T>(_ key : URLResourceKey) -> T? {
        return _values[key] as? T
    }
    private func _get(_ key : URLResourceKey) -> Bool? {
        return (_values[key] as? NSNumber)?.boolValue
    }
    private func _get(_ key: URLResourceKey) -> Int? {
        return (_values[key] as? NSNumber)?.intValue
    }
    
    private mutating func _set(_ key : URLResourceKey, newValue : Any?) {
        _keys.insert(key)
        _values[key] = newValue
    }
    private mutating func _set(_ key : URLResourceKey, newValue : String?) {
        _keys.insert(key)
        _values[key] = newValue as NSString?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : [String]?) {
        _keys.insert(key)
        _values[key] = newValue as NSObject?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : Date?) {
        _keys.insert(key)
        _values[key] = newValue as NSDate?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : URL?) {
        _keys.insert(key)
        _values[key] = newValue as NSURL?
    }
    private mutating func _set(_ key : URLResourceKey, newValue : Bool?) {
        _keys.insert(key)
        if let value = newValue {
            _values[key] = NSNumber(value: value)
        } else {
            _values[key] = nil
        }
    }
    private mutating func _set(_ key : URLResourceKey, newValue : Int?) {
        _keys.insert(key)
        if let value = newValue {
            _values[key] = NSNumber(value: value)
        } else {
            _values[key] = nil
        }
    }
    
    /// A loosely-typed dictionary containing all keys and values.
    ///
    /// If you have set temporary keys or non-standard keys, you can find them in here.
    public var allValues : [URLResourceKey : Any] {
        return _values
    }
    
    /// The resource name provided by the file system.
    public var name: String? {
        get { return _get(.nameKey) }
        set { _set(.nameKey, newValue: newValue) }
    }
    
    /// Localized or extension-hidden name as displayed to users.
    public var localizedName: String? { return _get(.localizedNameKey) }
    
    /// True for regular files.
    public var isRegularFile: Bool? { return _get(.isRegularFileKey) }
    
    /// True for directories.
    public var isDirectory: Bool? { return _get(.isDirectoryKey) }
    
    /// True for symlinks.
    public var isSymbolicLink: Bool? { return _get(.isSymbolicLinkKey) }
    
    /// True for the root directory of a volume.
    public var isVolume: Bool? { return _get(.isVolumeKey) }
    
    /// True for packaged directories. 
    ///
    /// - note: You can only set or clear this property on directories; if you try to set this property on non-directory objects, the property is ignored. If the directory is a package for some other reason (extension type, etc), setting this property to false will have no effect.
    public var isPackage: Bool? {
        get { return _get(.isPackageKey) }
        set { _set(.isPackageKey, newValue: newValue) }
    }
    
    /// True if resource is an application.
    @available(macOS 10.11, iOS 9.0, *)
    public var isApplication: Bool? { return _get(.isApplicationKey) }
    
#if os(macOS)
    /// True if the resource is scriptable. Only applies to applications.
    @available(macOS 10.11, *)
    public var applicationIsScriptable: Bool? { return _get(.applicationIsScriptableKey) }
#endif
    
    /// True for system-immutable resources.
    public var isSystemImmutable: Bool? { return _get(.isSystemImmutableKey) }
    
    /// True for user-immutable resources
    public var isUserImmutable: Bool? {
        get { return _get(.isUserImmutableKey) }
        set { _set(.isUserImmutableKey, newValue: newValue) }
    }
    
    /// True for resources normally not displayed to users.
    /// 
    /// - note: If the resource is a hidden because its name starts with a period, setting this property to false will not change the property.
    public var isHidden: Bool? {
        get { return _get(.isHiddenKey) }
        set { _set(.isHiddenKey, newValue: newValue) }
    }
    
    /// True for resources whose filename extension is removed from the localized name property.
    public var hasHiddenExtension: Bool? {
        get { return _get(.hasHiddenExtensionKey) }
        set { _set(.hasHiddenExtensionKey, newValue: newValue) }
    }
    
    /// The date the resource was created.
    public var creationDate: Date? {
        get { return _get(.creationDateKey) }
        set { _set(.creationDateKey, newValue: newValue) }
    }
    
    /// The date the resource was last accessed.
    public var contentAccessDate: Date? {
        get { return _get(.contentAccessDateKey) }
        set { _set(.contentAccessDateKey, newValue: newValue) }
    }
    
    /// The time the resource content was last modified.
    public var contentModificationDate: Date? {
        get { return _get(.contentModificationDateKey) }
        set { _set(.contentModificationDateKey, newValue: newValue) }
    }
    
    /// The time the resource's attributes were last modified.
    public var attributeModificationDate: Date? { return _get(.attributeModificationDateKey) }
    
    /// Number of hard links to the resource.
    public var linkCount: Int? { return _get(.linkCountKey) }
    
    /// The resource's parent directory, if any.
    public var parentDirectory: URL? { return _get(.parentDirectoryURLKey) }
    
    /// URL of the volume on which the resource is stored.
    public var volume: URL? { return _get(.volumeURLKey) }
    
    /// Uniform type identifier (UTI) for the resource.
    public var typeIdentifier: String? { return _get(.typeIdentifierKey) }
    
    /// User-visible type or "kind" description.
    public var localizedTypeDescription: String? { return _get(.localizedTypeDescriptionKey) }
    
    /// The label number assigned to the resource.
    public var labelNumber: Int? {
        get { return _get(.labelNumberKey) }
        set { _set(.labelNumberKey, newValue: newValue) }
    }
    
    
    /// The user-visible label text.
    public var localizedLabel: String? {
        get { return _get(.localizedLabelKey) }
    }
        
    /// An identifier which can be used to compare two file system objects for equality using `isEqual`.
    ///
    /// Two object identifiers are equal if they have the same file system path or if the paths are linked to same inode on the same file system. This identifier is not persistent across system restarts.
    public var fileResourceIdentifier: (NSCopying & NSCoding & NSSecureCoding & NSObjectProtocol)? { return _get(.fileResourceIdentifierKey) }
    
    /// An identifier that can be used to identify the volume the file system object is on. 
    ///
    /// Other objects on the same volume will have the same volume identifier and can be compared using for equality using `isEqual`. This identifier is not persistent across system restarts.
    public var volumeIdentifier: (NSCopying & NSCoding & NSSecureCoding & NSObjectProtocol)? { return _get(.volumeIdentifierKey) }
    
    /// The optimal block size when reading or writing this file's data, or nil if not available.
    public var preferredIOBlockSize: Int? { return _get(.preferredIOBlockSizeKey) }
    
    /// True if this process (as determined by EUID) can read the resource.
    public var isReadable: Bool? { return _get(.isReadableKey) }
    
    /// True if this process (as determined by EUID) can write to the resource.
    public var isWritable: Bool? { return _get(.isWritableKey) }
    
    /// True if this process (as determined by EUID) can execute a file resource or search a directory resource.
    public var isExecutable: Bool? { return _get(.isExecutableKey) }
    
    /// The file system object's security information encapsulated in a FileSecurity object.
    public var fileSecurity: NSFileSecurity? {
        get { return _get(.fileSecurityKey) }
        set { _set(.fileSecurityKey, newValue: newValue) }
    }
    
    /// True if resource should be excluded from backups, false otherwise.
    ///
    /// This property is only useful for excluding cache and other application support files which are not needed in a backup. Some operations commonly made to user documents will cause this property to be reset to false and so this property should not be used on user documents.
    public var isExcludedFromBackup: Bool? {
        get { return _get(.isExcludedFromBackupKey) }
        set { _set(.isExcludedFromBackupKey, newValue: newValue) }
    }
    
#if os(macOS)
    /// The array of Tag names.
    public var tagNames: [String]? { return _get(.tagNamesKey) }
#endif
    /// The URL's path as a file system path.
    public var path: String? { return _get(.pathKey) }
    
    /// The URL's path as a canonical absolute file system path.
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var canonicalPath: String? { return _get(.canonicalPathKey) }
    
    /// True if this URL is a file system trigger directory. Traversing or opening a file system trigger will cause an attempt to mount a file system on the trigger directory.
    public var isMountTrigger: Bool? { return _get(.isMountTriggerKey) }
    
    /// An opaque generation identifier which can be compared using `==` to determine if the data in a document has been modified.
    ///
    /// For URLs which refer to the same file inode, the generation identifier will change when the data in the file's data fork is changed (changes to extended attributes or other file system metadata do not change the generation identifier). For URLs which refer to the same directory inode, the generation identifier will change when direct children of that directory are added, removed or renamed (changes to the data of the direct children of that directory will not change the generation identifier). The generation identifier is persistent across system restarts. The generation identifier is tied to a specific document on a specific volume and is not transferred when the document is copied to another volume. This property is not supported by all volumes.
    @available(macOS 10.10, iOS 8.0, *)
    public var generationIdentifier: (NSCopying & NSCoding & NSSecureCoding & NSObjectProtocol)? { return _get(.generationIdentifierKey) }
    
    /// The document identifier -- a value assigned by the kernel to a document (which can be either a file or directory) and is used to identify the document regardless of where it gets moved on a volume.
    ///
    /// The document identifier survives "safe save" operations; i.e it is sticky to the path it was assigned to (`replaceItem(at:,withItemAt:,backupItemName:,options:,resultingItem:) throws` is the preferred safe-save API). The document identifier is persistent across system restarts. The document identifier is not transferred when the file is copied. Document identifiers are only unique within a single volume. This property is not supported by all volumes.
    @available(macOS 10.10, iOS 8.0, *)
    public var documentIdentifier: Int? { return _get(.documentIdentifierKey) }
    
    /// The date the resource was created, or renamed into or within its parent directory. Note that inconsistent behavior may be observed when this attribute is requested on hard-linked items. This property is not supported by all volumes.
    @available(macOS 10.10, iOS 8.0, *)
    public var addedToDirectoryDate: Date? { return _get(.addedToDirectoryDateKey) }
    
#if os(macOS)
    /// The quarantine properties as defined in LSQuarantine.h. To remove quarantine information from a file, pass `nil` as the value when setting this property.
    @available(macOS 10.10, *)
    public var quarantineProperties: [String : Any]? {
        get {
            let value = _values[.quarantinePropertiesKey]
            // If a caller has caused us to stash NSNull in the dictionary (via set), make sure to return nil instead of NSNull
            if value is NSNull {
                return nil
            } else {
                return value as? [String : Any]
            }
        }
        set {
            // Use NSNull for nil, a special case for deleting quarantine
            // properties
            _set(.quarantinePropertiesKey, newValue: newValue ?? NSNull())
        }
    }
#endif
    
    /// Returns the file system object type.
    public var fileResourceType: URLFileResourceType? { return _get(.fileResourceTypeKey) }
    
    /// The user-visible volume format.
    public var volumeLocalizedFormatDescription : String? { return _get(.volumeLocalizedFormatDescriptionKey) }
    
    /// Total volume capacity in bytes.
    public var volumeTotalCapacity : Int? { return _get(.volumeTotalCapacityKey) }
    
    /// Total free space in bytes.
    public var volumeAvailableCapacity : Int? { return _get(.volumeAvailableCapacityKey) }
    
#if os(macOS) || os(iOS)
    /// Total available capacity in bytes for "Important" resources, including space expected to be cleared by purging non-essential and cached resources. "Important" means something that the user or application clearly expects to be present on the local system, but is ultimately replaceable. This would include items that the user has explicitly requested via the UI, and resources that an application requires in order to provide functionality.
    /// Examples: A video that the user has explicitly requested to watch but has not yet finished watching or an audio file that the user has requested to download.
    /// This value should not be used in determining if there is room for an irreplaceable resource. In the case of irreplaceable resources, always attempt to save the resource regardless of available capacity and handle failure as gracefully as possible.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable) @available(watchOS, unavailable)
    public var volumeAvailableCapacityForImportantUsage: Int64? { return _get(.volumeAvailableCapacityForImportantUsageKey) }
    
    /// Total available capacity in bytes for "Opportunistic" resources, including space expected to be cleared by purging non-essential and cached resources. "Opportunistic" means something that the user is likely to want but does not expect to be present on the local system, but is ultimately non-essential and replaceable. This would include items that will be created or downloaded without an explicit request from the user on the current device.
    /// Examples: A background download of a newly available episode of a TV series that a user has been recently watching, a piece of content explicitly requested on another device, and a new document saved to a network server by the current user from another device.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable) @available(watchOS, unavailable)
    public var volumeAvailableCapacityForOpportunisticUsage: Int64? { return _get(.volumeAvailableCapacityForOpportunisticUsageKey) }
#endif
    
    /// Total number of resources on the volume.
    public var volumeResourceCount : Int? { return _get(.volumeResourceCountKey) }
    
    /// true if the volume format supports persistent object identifiers and can look up file system objects by their IDs.
    public var volumeSupportsPersistentIDs : Bool? { return _get(.volumeSupportsPersistentIDsKey) }
    
    /// true if the volume format supports symbolic links.
    public var volumeSupportsSymbolicLinks : Bool? { return _get(.volumeSupportsSymbolicLinksKey) }
    
    /// true if the volume format supports hard links.
    public var volumeSupportsHardLinks : Bool? { return _get(.volumeSupportsHardLinksKey) }
    
    /// true if the volume format supports a journal used to speed recovery in case of unplanned restart (such as a power outage or crash). This does not necessarily mean the volume is actively using a journal. 
    public var volumeSupportsJournaling : Bool? { return _get(.volumeSupportsJournalingKey) }
    
    /// true if the volume is currently using a journal for speedy recovery after an unplanned restart. 
    public var volumeIsJournaling : Bool? { return _get(.volumeIsJournalingKey) }
    
    /// true if the volume format supports sparse files, that is, files which can have 'holes' that have never been written to, and thus do not consume space on disk. A sparse file may have an allocated size on disk that is less than its logical length.
    public var volumeSupportsSparseFiles : Bool? { return _get(.volumeSupportsSparseFilesKey) }
    
    /// For security reasons, parts of a file (runs) that have never been written to must appear to contain zeroes. true if the volume keeps track of allocated but unwritten runs of a file so that it can substitute zeroes without actually writing zeroes to the media. 
    public var volumeSupportsZeroRuns : Bool? { return _get(.volumeSupportsZeroRunsKey) }

    /// true if the volume format treats upper and lower case characters in file and directory names as different. Otherwise an upper case character is equivalent to a lower case character, and you can't have two names that differ solely in the case of the characters. 
    public var volumeSupportsCaseSensitiveNames : Bool? { return _get(.volumeSupportsCaseSensitiveNamesKey) }

    /// true if the volume format preserves the case of file and directory names.  Otherwise the volume may change the case of some characters (typically making them all upper or all lower case). 
    public var volumeSupportsCasePreservedNames : Bool? { return _get(.volumeSupportsCasePreservedNamesKey) }

    /// true if the volume supports reliable storage of times for the root directory. 
    public var volumeSupportsRootDirectoryDates : Bool? { return _get(.volumeSupportsRootDirectoryDatesKey) }

    /// true if the volume supports returning volume size values (`volumeTotalCapacity` and `volumeAvailableCapacity`).
    public var volumeSupportsVolumeSizes : Bool? { return _get(.volumeSupportsVolumeSizesKey) }

    /// true if the volume can be renamed. 
    public var volumeSupportsRenaming : Bool? { return _get(.volumeSupportsRenamingKey) }

    /// true if the volume implements whole-file flock(2) style advisory locks, and the O_EXLOCK and O_SHLOCK flags of the open(2) call. 
    public var volumeSupportsAdvisoryFileLocking : Bool? { return _get(.volumeSupportsAdvisoryFileLockingKey) }

    /// true if the volume implements extended security (ACLs). 
    public var volumeSupportsExtendedSecurity : Bool? { return _get(.volumeSupportsExtendedSecurityKey) }

    /// true if the volume should be visible via the GUI (i.e., appear on the Desktop as a separate volume). 
    public var volumeIsBrowsable : Bool? { return _get(.volumeIsBrowsableKey) }

    /// The largest file size (in bytes) supported by this file system, or nil if this cannot be determined. 
    public var volumeMaximumFileSize : Int? { return _get(.volumeMaximumFileSizeKey) }

    /// true if the volume's media is ejectable from the drive mechanism under software control. 
    public var volumeIsEjectable : Bool? { return _get(.volumeIsEjectableKey) }

    /// true if the volume's media is removable from the drive mechanism. 
    public var volumeIsRemovable : Bool? { return _get(.volumeIsRemovableKey) }

    /// true if the volume's device is connected to an internal bus, false if connected to an external bus, or nil if not available. 
    public var volumeIsInternal : Bool? { return _get(.volumeIsInternalKey) }

    /// true if the volume is automounted. Note: do not mistake this with the functionality provided by kCFURLVolumeSupportsBrowsingKey. 
    public var volumeIsAutomounted : Bool? { return _get(.volumeIsAutomountedKey) }

    /// true if the volume is stored on a local device. 
    public var volumeIsLocal : Bool? { return _get(.volumeIsLocalKey) }

    /// true if the volume is read-only. 
    public var volumeIsReadOnly : Bool? { return _get(.volumeIsReadOnlyKey) }

    /// The volume's creation date, or nil if this cannot be determined. 
    public var volumeCreationDate : Date? { return _get(.volumeCreationDateKey) }

    /// The `URL` needed to remount a network volume, or nil if not available.
    public var volumeURLForRemounting : URL? { return _get(.volumeURLForRemountingKey) }

    /// The volume's persistent `UUID` as a string, or nil if a persistent `UUID` is not available for the volume.
    public var volumeUUIDString : String? { return _get(.volumeUUIDStringKey) }

    /// The name of the volume 
    public var volumeName : String? {
        get { return _get(.volumeNameKey) }
        set { _set(.volumeNameKey, newValue: newValue) }
    }
    
    /// The user-presentable name of the volume 
    public var volumeLocalizedName : String? { return _get(.volumeLocalizedNameKey) }
    
    /// true if the volume is encrypted. 
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeIsEncrypted : Bool? { return _get(.volumeIsEncryptedKey) }

    /// true if the volume is the root filesystem. 
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeIsRootFileSystem : Bool? { return _get(.volumeIsRootFileSystemKey) }

    /// true if the volume supports transparent decompression of compressed files using decmpfs. 
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeSupportsCompression : Bool? { return _get(.volumeSupportsCompressionKey) }
    
    /// true if the volume supports clonefile(2).
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeSupportsFileCloning : Bool? { return _get(.volumeSupportsFileCloningKey) }
    
    /// true if the volume supports renamex_np(2)'s RENAME_SWAP option.
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeSupportsSwapRenaming : Bool? { return _get(.volumeSupportsSwapRenamingKey) }
    
    /// true if the volume supports renamex_np(2)'s RENAME_EXCL option.
    @available(macOS 10.12, iOS 10.0, tvOS 10.0, watchOS 3.0, *)
    public var volumeSupportsExclusiveRenaming : Bool? { return _get(.volumeSupportsExclusiveRenamingKey) }
    
    /// true if the volume supports making files immutable with isUserImmutable or isSystemImmutable.
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, watchOS 4.0, *)
    public var volumeSupportsImmutableFiles : Bool? { return _get(.volumeSupportsImmutableFilesKey) }
    
    /// true if the volume supports setting POSIX access permissions with fileSecurity.
    @available(macOS 10.13, iOS 11.0, tvOS 11.0, watchOS 4.0, *)
    public var volumeSupportsAccessPermissions : Bool? { return _get(.volumeSupportsAccessPermissionsKey) }
    
    /// true if this item is synced to the cloud, false if it is only a local file. 
    public var isUbiquitousItem : Bool? { return _get(.isUbiquitousItemKey) }

    /// true if this item has conflicts outstanding. 
    public var ubiquitousItemHasUnresolvedConflicts : Bool? { return _get(.ubiquitousItemHasUnresolvedConflictsKey) }

    /// true if data is being downloaded for this item. 
    public var ubiquitousItemIsDownloading : Bool? { return _get(.ubiquitousItemIsDownloadingKey) }

    /// true if there is data present in the cloud for this item. 
    public var ubiquitousItemIsUploaded : Bool? { return _get(.ubiquitousItemIsUploadedKey) }

    /// true if data is being uploaded for this item. 
    public var ubiquitousItemIsUploading : Bool? { return _get(.ubiquitousItemIsUploadingKey) }
    
    /// returns the download status of this item.
    public var ubiquitousItemDownloadingStatus : URLUbiquitousItemDownloadingStatus? { return _get(.ubiquitousItemDownloadingStatusKey) }
    
    /// returns the error when downloading the item from iCloud failed, see the NSUbiquitousFile section in FoundationErrors.h
    public var ubiquitousItemDownloadingError : NSError? { return _get(.ubiquitousItemDownloadingErrorKey) }

    /// returns the error when uploading the item to iCloud failed, see the NSUbiquitousFile section in FoundationErrors.h
    public var ubiquitousItemUploadingError : NSError? { return _get(.ubiquitousItemUploadingErrorKey) }
    
    /// returns whether a download of this item has already been requested with an API like `startDownloadingUbiquitousItem(at:) throws`.
    @available(macOS 10.10, iOS 8.0, *)
    public var ubiquitousItemDownloadRequested : Bool? { return _get(.ubiquitousItemDownloadRequestedKey) }
    
    /// returns the name of this item's container as displayed to users.
    @available(macOS 10.10, iOS 8.0, *)
    public var ubiquitousItemContainerDisplayName : String? { return _get(.ubiquitousItemContainerDisplayNameKey) }
    
#if os(macOS) || os(iOS)
    // true if ubiquitous item is shared.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable) @available(watchOS, unavailable)
    public var ubiquitousItemIsShared: Bool? { return _get(.ubiquitousItemIsSharedKey) }
    
    // The current user's role for this shared item, or nil if not shared
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable) @available(watchOS, unavailable)
    public var ubiquitousSharedItemCurrentUserRole: URLUbiquitousSharedItemRole? { return _get(.ubiquitousSharedItemCurrentUserRoleKey) }
    
    // The permissions for the current user, or nil if not shared.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable) @available(watchOS, unavailable)
    public var ubiquitousSharedItemCurrentUserPermissions: URLUbiquitousSharedItemPermissions? { return _get(.ubiquitousSharedItemCurrentUserPermissionsKey) }
    
    // The name components for the owner, or nil if not shared.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable) @available(watchOS, unavailable)
    public var ubiquitousSharedItemOwnerNameComponents: PersonNameComponents? { return _get(.ubiquitousSharedItemOwnerNameComponentsKey) }
    
    // The name components for the most recent editor, or nil if not shared.
    @available(macOS 10.13, iOS 11.0, *) @available(tvOS, unavailable) @available(watchOS, unavailable)
    public var ubiquitousSharedItemMostRecentEditorNameComponents: PersonNameComponents? { return _get(.ubiquitousSharedItemMostRecentEditorNameComponentsKey) }
#endif
    
#if !os(macOS)
    /// The protection level for this file
    @available(iOS 9.0, *)
    public var fileProtection : URLFileProtection? { return _get(.fileProtectionKey) }
#endif
    
    /// Total file size in bytes
    ///
    /// - note: Only applicable to regular files.
    public var fileSize : Int? { return _get(.fileSizeKey) }
    
    /// Total size allocated on disk for the file in bytes (number of blocks times block size)
    ///
    /// - note: Only applicable to regular files.
    public var fileAllocatedSize : Int? { return _get(.fileAllocatedSizeKey) }
    
    /// Total displayable size of the file in bytes (this may include space used by metadata), or nil if not available.
    ///
    /// - note: Only applicable to regular files.
    public var totalFileSize : Int? { return _get(.totalFileSizeKey) }
    
    /// Total allocated size of the file in bytes (this may include space used by metadata), or nil if not available. This can be less than the value returned by `totalFileSize` if the resource is compressed.
    ///
    /// - note: Only applicable to regular files.
    public var totalFileAllocatedSize : Int? { return _get(.totalFileAllocatedSizeKey) }

    /// true if the resource is a Finder alias file or a symlink, false otherwise
    ///
    /// - note: Only applicable to regular files.
    public var isAliasFile : Bool? { return _get(.isAliasFileKey) }

}

/**
 A URL is a type that can potentially contain the location of a resource on a remote server, the path of a local file on disk, or even an arbitrary piece of encoded data.
 
 You can construct URLs and access their parts. For URLs that represent local files, you can also manipulate properties of those files directly, such as changing the file's last modification date. Finally, you can pass URLs to other APIs to retrieve the contents of those URLs. For example, you can use the URLSession classes to access the contents of remote resources, as described in URL Session Programming Guide.
 
 URLs are the preferred way to refer to local files. Most objects that read data from or write data to a file have methods that accept a URL instead of a pathname as the file reference. For example, you can get the contents of a local file URL as `String` by calling `func init(contentsOf:encoding) throws`, or as a `Data` by calling `func init(contentsOf:options) throws`.
*/
public struct URL : ReferenceConvertible, Equatable {
    public typealias ReferenceType = NSURL
    fileprivate var _url : NSURL
    
    public typealias BookmarkResolutionOptions = NSURL.BookmarkResolutionOptions
    public typealias BookmarkCreationOptions = NSURL.BookmarkCreationOptions

    /// Initialize with string.
    ///
    /// Returns `nil` if a `URL` cannot be formed with the string (for example, if the string contains characters that are illegal in a URL, or is an empty string).
    public init?(string: String) {
        guard !string.isEmpty else { return nil }
        
        if let inner = NSURL(string: string) {
            _url = URL._converted(from: inner)
        } else {
            return nil
        }
    }
    
    /// Initialize with string, relative to another URL.
    ///
    /// Returns `nil` if a `URL` cannot be formed with the string (for example, if the string contains characters that are illegal in a URL, or is an empty string).
    public init?(string: String, relativeTo url: URL?) {
        guard !string.isEmpty else { return nil }
        
        if let inner = NSURL(string: string, relativeTo: url) {
            _url = URL._converted(from: inner)
        } else {
            return nil
        }
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path, relative to a base URL.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    /// - note: This function avoids an extra file system access to check if the file URL is a directory. You should use it if you know the answer already.
    @available(macOS 10.11, iOS 9.0, *)
    public init(fileURLWithPath path: String, isDirectory: Bool, relativeTo base: URL?) {
        _url = URL._converted(from: NSURL(fileURLWithPath: path.isEmpty ? "." : path, isDirectory: isDirectory, relativeTo: base))
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path, relative to a base URL.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    @available(macOS 10.11, iOS 9.0, *)
    public init(fileURLWithPath path: String, relativeTo base: URL?) {
        _url = URL._converted(from: NSURL(fileURLWithPath: path.isEmpty ? "." : path, relativeTo: base))
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    /// - note: This function avoids an extra file system access to check if the file URL is a directory. You should use it if you know the answer already.
    public init(fileURLWithPath path: String, isDirectory: Bool) {
        _url = URL._converted(from: NSURL(fileURLWithPath: path.isEmpty ? "." : path, isDirectory: isDirectory))
    }
    
    /// Initializes a newly created file URL referencing the local file or directory at path.
    ///
    /// If an empty string is used for the path, then the path is assumed to be ".".
    public init(fileURLWithPath path: String) {
        _url = URL._converted(from: NSURL(fileURLWithPath: path.isEmpty ? "." : path))
    }
    
    /// Initializes a newly created URL using the contents of the given data, relative to a base URL.
    ///
    /// If the data representation is not a legal URL string as ASCII bytes, the URL object may not behave as expected. If the URL cannot be formed then this will return nil.
    @available(macOS 10.11, iOS 9.0, *)
    public init?(dataRepresentation: Data, relativeTo url: URL?, isAbsolute: Bool = false) {
        guard dataRepresentation.count > 0 else { return nil }
        
        if isAbsolute {
            _url = URL._converted(from: NSURL(absoluteURLWithDataRepresentation: dataRepresentation, relativeTo: url))
        } else {
            _url = URL._converted(from: NSURL(dataRepresentation: dataRepresentation, relativeTo: url))
        }
    }

    /// Initializes a URL that refers to a location specified by resolving bookmark data.
    @available(swift, obsoleted: 4.2)
    public init?(resolvingBookmarkData data: Data, options: BookmarkResolutionOptions = [], relativeTo url: URL? = nil, bookmarkDataIsStale: inout Bool) throws {
        var stale : ObjCBool = false
        _url = URL._converted(from: try NSURL(resolvingBookmarkData: data, options: options, relativeTo: url, bookmarkDataIsStale: &stale))
        bookmarkDataIsStale = stale.boolValue
    }

    /// Initializes a URL that refers to a location specified by resolving bookmark data.
    @available(swift, introduced: 4.2)
    public init(resolvingBookmarkData data: Data, options: BookmarkResolutionOptions = [], relativeTo url: URL? = nil, bookmarkDataIsStale: inout Bool) throws {
        var stale : ObjCBool = false
        _url = URL._converted(from: try NSURL(resolvingBookmarkData: data, options: options, relativeTo: url, bookmarkDataIsStale: &stale))
        bookmarkDataIsStale = stale.boolValue
    }
    
    /// Creates and initializes an NSURL that refers to the location specified by resolving the alias file at url. If the url argument does not refer to an alias file as defined by the NSURLIsAliasFileKey property, the NSURL returned is the same as url argument. This method fails and returns nil if the url argument is unreachable, or if the original file or directory could not be located or is not reachable, or if the original file or directory is on a volume that could not be located or mounted. The URLBookmarkResolutionWithSecurityScope option is not supported by this method.
    @available(macOS 10.10, iOS 8.0, *)
    public init(resolvingAliasFileAt url: URL, options: BookmarkResolutionOptions = []) throws {
        self.init(reference: try NSURL(resolvingAliasFileAt: url, options: options))
    }

    /// Initializes a newly created URL referencing the local file or directory at the file system representation of the path. File system representation is a null-terminated C string with canonical UTF-8 encoding.
    public init(fileURLWithFileSystemRepresentation path: UnsafePointer<Int8>, isDirectory: Bool, relativeTo baseURL: URL?) {
        _url = URL._converted(from: NSURL(fileURLWithFileSystemRepresentation: path, isDirectory: isDirectory, relativeTo: baseURL))
    }
    
    public var hashValue: Int {
        return _url.hash
    }
    
    // MARK: -
    
    /// Returns the data representation of the URL's relativeString. 
    ///
    /// If the URL was initialized with `init?(dataRepresentation:relativeTo:isAbsolute:)`, the data representation returned are the same bytes as those used at initialization; otherwise, the data representation returned are the bytes of the `relativeString` encoded with UTF8 string encoding.
    @available(macOS 10.11, iOS 9.0, *)
    public var dataRepresentation: Data {
        return _url.dataRepresentation
    }
    
    // MARK: -
    
    // Future implementation note:
    // NSURL (really CFURL, which provides its implementation) has quite a few quirks in its processing of some more esoteric (and some not so esoteric) strings. We would like to move much of this over to the more modern NSURLComponents, but binary compat concerns have made this difficult.
    // Hopefully soon, we can replace some of the below delegation to NSURL with delegation to NSURLComponents instead. It cannot be done piecemeal, because otherwise we will get inconsistent results from the API.
    
    /// Returns the absolute string for the URL.
    public var absoluteString: String {
        if let string = _url.absoluteString {
            return string
        } else {
            // This should never fail for non-file reference URLs
            return ""
        }
    }
    
    /// The relative portion of a URL.
    ///
    /// If `baseURL` is nil, or if the receiver is itself absolute, this is the same as `absoluteString`.
    public var relativeString: String {
        return _url.relativeString
    }
    
    /// Returns the base URL.
    ///
    /// If the URL is itself absolute, then this value is nil.
    public var baseURL: URL? {
        return _url.baseURL
    }
    
    /// Returns the absolute URL.
    ///
    /// If the URL is itself absolute, this will return self.
    public var absoluteURL: URL {
        if let url = _url.absoluteURL {
            return url
        } else {
            // This should never fail for non-file reference URLs
            return self
        }
    }
    
    // MARK: -
    
    /// Returns the scheme of the URL.
    public var scheme: String? {
        return _url.scheme
    }
    
    /// Returns true if the scheme is `file:`.
    public var isFileURL: Bool {
        return _url.isFileURL
    }

    // This thing was never really part of the URL specs
    @available(*, unavailable, message: "Use `path`, `query`, and `fragment` instead")
    public var resourceSpecifier: String {
        fatalError()
    }
    
    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the host component of the URL; otherwise it returns nil.
    ///
    /// - note: This function will resolve against the base `URL`.
    public var host: String? {
        return _url.host
    }
    
    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the port component of the URL; otherwise it returns nil.
    ///
    /// - note: This function will resolve against the base `URL`.
    public var port: Int? {
        return _url.port?.intValue
    }
    
    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the user component of the URL; otherwise it returns nil.
    ///
    /// - note: This function will resolve against the base `URL`.
    public var user: String? {
        return _url.user
    }
    
    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the password component of the URL; otherwise it returns nil.
    ///
    /// - note: This function will resolve against the base `URL`.
    public var password: String? {
        return _url.password
    }
    
    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the path component of the URL; otherwise it returns an empty string.
    ///
    /// If the URL contains a parameter string, it is appended to the path with a `;`.
    /// - note: This function will resolve against the base `URL`.
    /// - returns: The path, or an empty string if the URL has an empty path.
    public var path: String {
        if let parameterString = _url.parameterString {
            if let path = _url.path {
                return path + ";" + parameterString
            } else {
                return ";" + parameterString
            }
        } else if let path = _url.path {
            return path
        } else {
            return ""
        }
    }
    
    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the relative path of the URL; otherwise it returns nil.
    ///
    /// This is the same as path if baseURL is nil.
    /// If the URL contains a parameter string, it is appended to the path with a `;`.
    ///
    /// - note: This function will resolve against the base `URL`.
    /// - returns: The relative path, or an empty string if the URL has an empty path.
    public var relativePath: String {
        if let parameterString = _url.parameterString {
            if let path = _url.relativePath {
                return path + ";" + parameterString
            } else {
                return ";" + parameterString
            }
        } else if let path = _url.relativePath {
            return path
        } else {
            return ""
        }
    }

    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the fragment component of the URL; otherwise it returns nil.
    ///
    /// - note: This function will resolve against the base `URL`.
    public var fragment: String? {
        return _url.fragment
    }
    
    @available(*, unavailable, message: "use the 'path' property")
    public var parameterString: String? {
        fatalError()
    }
    
    /// If the URL conforms to RFC 1808 (the most common form of URL), returns the query of the URL; otherwise it returns nil.
    ///
    /// - note: This function will resolve against the base `URL`.
    public var query: String? {
        return _url.query
    }
    
    /// Returns true if the URL path represents a directory.
    @available(macOS 10.11, iOS 9.0, *)
    public var hasDirectoryPath: Bool {
        return _url.hasDirectoryPath
    }
    
    /// Passes the URL's path in file system representation to `block`. 
    /// 
    /// File system representation is a null-terminated C string with canonical UTF-8 encoding.
    /// - note: The pointer is not valid outside the context of the block.
    @available(macOS 10.9, iOS 7.0, *)
    public func withUnsafeFileSystemRepresentation<ResultType>(_ block: (UnsafePointer<Int8>?) throws -> ResultType) rethrows -> ResultType {
        return try block(_url.fileSystemRepresentation)
    }
    
    // MARK: -
    // MARK: Path manipulation
    
    /// Returns the path components of the URL, or an empty array if the path is an empty string.
    public var pathComponents: [String] {
        // In accordance with our above change to never return a nil path, here we return an empty array.
        return _url.pathComponents ?? []
    }
    
    /// Returns the last path component of the URL, or an empty string if the path is an empty string.
    public var lastPathComponent: String {
        return _url.lastPathComponent ?? ""
    }
    
    /// Returns the path extension of the URL, or an empty string if the path is an empty string.
    public var pathExtension: String {
        return _url.pathExtension ?? ""
    }
    
    /// Returns a URL constructed by appending the given path component to self.
    ///
    /// - parameter pathComponent: The path component to add.
    /// - parameter isDirectory: If `true`, then a trailing `/` is added to the resulting path.
    public func appendingPathComponent(_ pathComponent: String, isDirectory: Bool) -> URL {
        if let result = _url.appendingPathComponent(pathComponent, isDirectory: isDirectory) {
            return result
        } else {
            // Now we need to do something more expensive
            if var c = URLComponents(url: self, resolvingAgainstBaseURL: true) {
                c.path = (c.path as NSString).appendingPathComponent(pathComponent)
                
                if let result = c.url {
                    return result
                } else {
                    // Couldn't get url from components
                    // Ultimate fallback:
                    return self
                }
            } else {
                return self
            }
        }
    }
    
    /// Returns a URL constructed by appending the given path component to self.
    ///
    /// - note: This function performs a file system operation to determine if the path component is a directory. If so, it will append a trailing `/`. If you know in advance that the path component is a directory or not, then use `func appendingPathComponent(_:isDirectory:)`.
    /// - parameter pathComponent: The path component to add.
    public func appendingPathComponent(_ pathComponent: String) -> URL {
        if let result = _url.appendingPathComponent(pathComponent) {
            return result
        } else {
            // Now we need to do something more expensive
            if var c = URLComponents(url: self, resolvingAgainstBaseURL: true) {
                c.path = (c.path as NSString).appendingPathComponent(pathComponent)
                
                if let result = c.url {
                    return result
                } else {
                    // Couldn't get url from components
                    // Ultimate fallback:
                    return self
                }
            } else {
                // Ultimate fallback:
                return self
            }
        }
    }
    
    /// Returns a URL constructed by removing the last path component of self.
    ///
    /// This function may either remove a path component or append `/..`.
    ///
    /// If the URL has an empty path (e.g., `http://www.example.com`), then this function will return the URL unchanged.
    public func deletingLastPathComponent() -> URL {
        // This is a slight behavior change from NSURL, but better than returning "http://www.example.com../".
        if path.isEmpty {
            return self
        }
        
        if let result = _url.deletingLastPathComponent.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            return self
        }
    }
    
    /// Returns a URL constructed by appending the given path extension to self.
    ///
    /// If the URL has an empty path (e.g., `http://www.example.com`), then this function will return the URL unchanged.
    ///
    /// Certain special characters (for example, Unicode Right-To-Left marks) cannot be used as path extensions. If any of those are contained in `pathExtension`, the function will return the URL unchanged.
    /// - parameter pathExtension: The extension to append.
    public func appendingPathExtension(_ pathExtension: String) -> URL {
        if path.isEmpty {
            return self
        }
        
        if let result = _url.appendingPathExtension(pathExtension) {
            return result
        } else {
            return self
        }
    }
    
    /// Returns a URL constructed by removing any path extension.
    ///
    /// If the URL has an empty path (e.g., `http://www.example.com`), then this function will return the URL unchanged.
    public func deletingPathExtension() -> URL {
        if path.isEmpty {
            return self
        }

        if let result = _url.deletingPathExtension.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            return self
        }
    }

    /// Appends a path component to the URL.
    ///
    /// - parameter pathComponent: The path component to add.
    /// - parameter isDirectory: Use `true` if the resulting path is a directory.
    public mutating func appendPathComponent(_ pathComponent: String, isDirectory: Bool) {
        self = appendingPathComponent(pathComponent, isDirectory: isDirectory)
    }
    
    /// Appends a path component to the URL.
    ///
    /// - note: This function performs a file system operation to determine if the path component is a directory. If so, it will append a trailing `/`. If you know in advance that the path component is a directory or not, then use `func appendingPathComponent(_:isDirectory:)`.
    /// - parameter pathComponent: The path component to add.
    public mutating func appendPathComponent(_ pathComponent: String) {
        self = appendingPathComponent(pathComponent)
    }
    
    /// Appends the given path extension to self.
    ///
    /// If the URL has an empty path (e.g., `http://www.example.com`), then this function will do nothing.
    /// Certain special characters (for example, Unicode Right-To-Left marks) cannot be used as path extensions. If any of those are contained in `pathExtension`, the function will return the URL unchanged.
    /// - parameter pathExtension: The extension to append.
    public mutating func appendPathExtension(_ pathExtension: String) {
        self = appendingPathExtension(pathExtension)
    }

    /// Returns a URL constructed by removing the last path component of self.
    ///
    /// This function may either remove a path component or append `/..`.
    ///
    /// If the URL has an empty path (e.g., `http://www.example.com`), then this function will do nothing.
    public mutating func deleteLastPathComponent() {
        self = deletingLastPathComponent()
    }
    

    /// Returns a URL constructed by removing any path extension.
    ///
    /// If the URL has an empty path (e.g., `http://www.example.com`), then this function will do nothing.
    public mutating func deletePathExtension() {
        self = deletingPathExtension()
    }
    
    /// Returns a `URL` with any instances of ".." or "." removed from its path.
    public var standardized : URL {
        // The NSURL API can only return nil in case of file reference URL, which we should not be
        if let result = _url.standardized.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            return self
        }
    }
    
    /// Standardizes the path of a file URL.
    ///
    /// If the `isFileURL` is false, this method does nothing.
    public mutating func standardize() {
        self = self.standardized
    }
    
    /// Standardizes the path of a file URL.
    ///
    /// If the `isFileURL` is false, this method returns `self`.
    public var standardizedFileURL : URL {
        // NSURL should not return nil here unless this is a file reference URL, which should be impossible
        if let result = _url.standardizingPath.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            return self
        }
    }
    
    /// Resolves any symlinks in the path of a file URL.
    ///
    /// If the `isFileURL` is false, this method returns `self`.
    public func resolvingSymlinksInPath() -> URL {
        // NSURL should not return nil here unless this is a file reference URL, which should be impossible
        if let result = _url.resolvingSymlinksInPath.map({ URL(reference: $0 as NSURL) }) {
            return result
        } else {
            return self
        }
    }

    /// Resolves any symlinks in the path of a file URL.
    ///
    /// If the `isFileURL` is false, this method does nothing.
    public mutating func resolveSymlinksInPath() {
        self = self.resolvingSymlinksInPath()
    }

    // MARK: - Reachability
    
    /// Returns whether the URL's resource exists and is reachable. 
    ///
    /// This method synchronously checks if the resource's backing store is reachable. Checking reachability is appropriate when making decisions that do not require other immediate operations on the resource, e.g. periodic maintenance of UI state that depends on the existence of a specific document. When performing operations such as opening a file or copying resource properties, it is more efficient to simply try the operation and handle failures. This method is currently applicable only to URLs for file system resources. For other URL types, `false` is returned.
    public func checkResourceIsReachable() throws -> Bool {
        var error : NSError?
        let result = _url.checkResourceIsReachableAndReturnError(&error)
        if let e = error {
            throw e
        } else {
            return result
        }
    }
    
    /// Returns whether the promised item URL's resource exists and is reachable.
    ///
    /// This method synchronously checks if the resource's backing store is reachable. Checking reachability is appropriate when making decisions that do not require other immediate operations on the resource, e.g. periodic maintenance of UI state that depends on the existence of a specific document. When performing operations such as opening a file or copying resource properties, it is more efficient to simply try the operation and handle failures. This method is currently applicable only to URLs for file system resources. For other URL types, `false` is returned.
    @available(macOS 10.10, iOS 8.0, *)
    public func checkPromisedItemIsReachable() throws -> Bool {
        var error : NSError?
        let result = _url.checkPromisedItemIsReachableAndReturnError(&error)
        if let e = error {
            throw e
        } else {
            return result
        }
    }
    
    // MARK: - Resource Values
    
    /// Sets the resource value identified by a given resource key.
    /// 
    /// This method writes the new resource values out to the backing store. Attempts to set a read-only resource property or to set a resource property not supported by the resource are ignored and are not considered errors. This method is currently applicable only to URLs for file system resources.
    ///
    /// `URLResourceValues` keeps track of which of its properties have been set. Those values are the ones used by this function to determine which properties to write.
    public mutating func setResourceValues(_ values: URLResourceValues) throws {
        try _url.setResourceValues(values._values)
    }
    
    /// Return a collection of resource values identified by the given resource keys.
    ///
    /// This method first checks if the URL object already caches the resource value. If so, it returns the cached resource value to the caller. If not, then this method synchronously obtains the resource value from the backing store, adds the resource value to the URL object's cache, and returns the resource value to the caller. The type of the resource value varies by resource property (see resource key definitions). If this method does not throw and the resulting value in the `URLResourceValues` is populated with nil, it means the resource property is not available for the specified resource and no errors occurred when determining the resource property was not available. This method is currently applicable only to URLs for file system resources.
    ///
    /// When this function is used from the main thread, resource values cached by the URL (except those added as temporary properties) are removed the next time the main thread's run loop runs. `func removeCachedResourceValue(forKey:)` and `func removeAllCachedResourceValues()` also may be used to remove cached resource values.
    ///
    /// Only the values for the keys specified in `keys` will be populated.
    public func resourceValues(forKeys keys: Set<URLResourceKey>) throws -> URLResourceValues {
        return URLResourceValues(keys: keys, values: try _url.resourceValues(forKeys: Array(keys)))
    }

    /// Sets a temporary resource value on the URL object.
    ///
    /// Temporary resource values are for client use. Temporary resource values exist only in memory and are never written to the resource's backing store. Once set, a temporary resource value can be copied from the URL object with `func resourceValues(forKeys:)`. The values are stored in the loosely-typed `allValues` dictionary property.
    ///
    /// To remove a temporary resource value from the URL object, use `func removeCachedResourceValue(forKey:)`. Care should be taken to ensure the key that identifies a temporary resource value is unique and does not conflict with system defined keys (using reverse domain name notation in your temporary resource value keys is recommended). This method is currently applicable only to URLs for file system resources.
    public mutating func setTemporaryResourceValue(_ value : Any, forKey key: URLResourceKey) {
        _url.setTemporaryResourceValue(value, forKey: key)
    }
    
    /// Removes all cached resource values and all temporary resource values from the URL object.
    /// 
    /// This method is currently applicable only to URLs for file system resources.
    public mutating func removeAllCachedResourceValues() {
        _url.removeAllCachedResourceValues()
    }
    
    /// Removes the cached resource value identified by a given resource value key from the URL object.
    /// 
    /// Removing a cached resource value may remove other cached resource values because some resource values are cached as a set of values, and because some resource values depend on other resource values (temporary resource values have no dependencies). This method is currently applicable only to URLs for file system resources.
    public mutating func removeCachedResourceValue(forKey key: URLResourceKey) {
        _url.removeCachedResourceValue(forKey: key)
    }
    
    /// Get resource values from URLs of 'promised' items.
    ///
    /// A promised item is not guaranteed to have its contents in the file system until you use `FileCoordinator` to perform a coordinated read on its URL, which causes the contents to be downloaded or otherwise generated. Promised item URLs are returned by various APIs, including currently:
    ///     NSMetadataQueryUbiquitousDataScope
    ///     NSMetadataQueryUbiquitousDocumentsScope
    ///     A `FilePresenter` presenting the contents of the directory located by -URLForUbiquitousContainerIdentifier: or a subdirectory thereof
    ///
    /// The following methods behave identically to their similarly named methods above (`func resourceValues(forKeys:)`, etc.), except that they allow you to get resource values and check for presence regardless of whether the promised item's contents currently exist at the URL. You must use these APIs instead of the normal URL resource value APIs if and only if any of the following are true:
    ///     You are using a URL that you know came directly from one of the above APIs
    ///     You are inside the accessor block of a coordinated read or write that used NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly, NSFileCoordinatorWritingForDeleting, NSFileCoordinatorWritingForMoving, or NSFileCoordinatorWritingContentIndependentMetadataOnly
    ///
    /// Most of the URL resource value keys will work with these APIs. However, there are some that are tied to the item's contents that will not work, such as `contentAccessDateKey` or `generationIdentifierKey`. If one of these keys is used, the method will return a `URLResourceValues` value, but the value for that property will be nil.
    @available(macOS 10.10, iOS 8.0, *)
    public func promisedItemResourceValues(forKeys keys: Set<URLResourceKey>) throws -> URLResourceValues {
        return URLResourceValues(keys: keys, values: try _url.promisedItemResourceValues(forKeys: Array(keys)))
    }
    
    @available(*, unavailable, message: "Use struct URLResourceValues and URL.setResourceValues(_:) instead")
    public func setResourceValue(_ value: AnyObject?, forKey key: URLResourceKey) throws {
        fatalError()
    }
    
    @available(*, unavailable, message: "Use struct URLResourceValues and URL.setResourceValues(_:) instead")
    public func setResourceValues(_ keyedValues: [URLResourceKey : AnyObject]) throws {
        fatalError()
    }
        
    @available(*, unavailable, message: "Use struct URLResourceValues and URL.setResourceValues(_:) instead")
    public func getResourceValue(_ value: AutoreleasingUnsafeMutablePointer<AnyObject?>, forKey key: URLResourceKey) throws {
        fatalError()
    }
    
    // MARK: - Bookmarks and Alias Files
    
    /// Returns bookmark data for the URL, created with specified options and resource values.
    public func bookmarkData(options: BookmarkCreationOptions = [], includingResourceValuesForKeys keys: Set<URLResourceKey>? = nil, relativeTo url: URL? = nil) throws -> Data {
        let result = try _url.bookmarkData(options: options, includingResourceValuesForKeys: keys.flatMap { Array($0) }, relativeTo: url)
        return result as Data
    }
    
    /// Returns the resource values for properties identified by a specified array of keys contained in specified bookmark data. If the result dictionary does not contain a resource value for one or more of the requested resource keys, it means those resource properties are not available in the bookmark data.
    public static func resourceValues(forKeys keys: Set<URLResourceKey>, fromBookmarkData data: Data) -> URLResourceValues? {
        return NSURL.resourceValues(forKeys: Array(keys), fromBookmarkData: data).map { URLResourceValues(keys: keys, values: $0) }
    }
    
    /// Creates an alias file on disk at a specified location with specified bookmark data. bookmarkData must have been created with the URLBookmarkCreationSuitableForBookmarkFile option. bookmarkFileURL must either refer to an existing file (which will be overwritten), or to location in an existing directory.
    public static func writeBookmarkData(_ data : Data, to url: URL) throws {
        // Options are unused
        try NSURL.writeBookmarkData(data, to: url, options: 0)
    }
 
    /// Initializes and returns bookmark data derived from an alias file pointed to by a specified URL. If bookmarkFileURL refers to an alias file created prior to OS X v10.6 that contains Alias Manager information but no bookmark data, this method synthesizes bookmark data for the file.
    public static func bookmarkData(withContentsOf url: URL) throws -> Data {
        let result = try NSURL.bookmarkData(withContentsOf: url)
        return result as Data
    }
    
    /// Given an NSURL created by resolving a bookmark data created with security scope, make the resource referenced by the url accessible to the process. When access to this resource is no longer needed the client must call stopAccessingSecurityScopedResource. Each call to startAccessingSecurityScopedResource must be balanced with a call to stopAccessingSecurityScopedResource (Note: this is not reference counted).
    @available(macOS 10.7, iOS 8.0, *)
    public func startAccessingSecurityScopedResource() -> Bool {
        return _url.startAccessingSecurityScopedResource()
    }
    
    /// Revokes the access granted to the url by a prior successful call to startAccessingSecurityScopedResource.
    @available(macOS 10.7, iOS 8.0, *)
    public func stopAccessingSecurityScopedResource() {
        _url.stopAccessingSecurityScopedResource()
    }
    
    // MARK: - Bridging Support
    
    /// We must not store an NSURL without running it through this function. This makes sure that we do not hold a file reference URL, which changes the nullability of many NSURL functions.
    private static func _converted(from url: NSURL) -> NSURL {
        // Future readers: file reference URL here is not the same as playgrounds "file reference"
        if url.isFileReferenceURL() {
            // Convert to a file path URL, or use an invalid scheme
            return (url.filePathURL ?? URL(string: "com-apple-unresolvable-file-reference-url:")!) as NSURL
        } else {
            return url
        }
    }
    
    fileprivate init(reference: NSURL) {
        _url = URL._converted(from: reference).copy() as! NSURL
    }
    
    private var reference : NSURL {
        return _url
    }

    public static func ==(lhs: URL, rhs: URL) -> Bool {
        return lhs.reference.isEqual(rhs.reference)
    }
}

extension URL : _ObjectiveCBridgeable {
    @_semantics("convertToObjectiveC")
    public func _bridgeToObjectiveC() -> NSURL {
        return _url
    }
    
    public static func _forceBridgeFromObjectiveC(_ source: NSURL, result: inout URL?) {
        if !_conditionallyBridgeFromObjectiveC(source, result: &result) {
            fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
        }
    }
    
    public static func _conditionallyBridgeFromObjectiveC(_ source: NSURL, result: inout URL?) -> Bool {
        result = URL(reference: source)
        return true
    }

    public static func _unconditionallyBridgeFromObjectiveC(_ source: NSURL?) -> URL {
        var result: URL?
        _forceBridgeFromObjectiveC(source!, result: &result)
        return result!
    }
}

extension URL : CustomStringConvertible, CustomDebugStringConvertible {
    public var description: String {
        return _url.description
    }
    
    public var debugDescription: String {
        return _url.debugDescription
    }
}

extension NSURL : _HasCustomAnyHashableRepresentation {
    // Must be @nonobjc to avoid infinite recursion during bridging.
    @nonobjc
    public func _toCustomAnyHashable() -> AnyHashable? {
        return AnyHashable(self as URL)
    }
}

extension URL : _CustomPlaygroundQuickLookable {
    @available(*, deprecated, message: "URL.customPlaygroundQuickLook will be removed in a future Swift version")
    public var customPlaygroundQuickLook: PlaygroundQuickLook {
        return .url(absoluteString)
    }
}

extension URL : Codable {
    private enum CodingKeys : Int, CodingKey {
        case base
        case relative
    }

    public init(from decoder: Decoder) throws {
        let container = try decoder.container(keyedBy: CodingKeys.self)
        let relative = try container.decode(String.self, forKey: .relative)
        let base = try container.decodeIfPresent(URL.self, forKey: .base)

        guard let url = URL(string: relative, relativeTo: base) else {
            throw DecodingError.dataCorrupted(DecodingError.Context(codingPath: decoder.codingPath,
                                                                    debugDescription: "Invalid URL string."))
        }

        self = url
    }

    public func encode(to encoder: Encoder) throws {
        var container = encoder.container(keyedBy: CodingKeys.self)
        try container.encode(self.relativeString, forKey: .relative)
        if let base = self.baseURL {
            try container.encode(base, forKey: .base)
        }
    }
}

//===----------------------------------------------------------------------===//
// File references, for playgrounds.
//===----------------------------------------------------------------------===//

extension URL : _ExpressibleByFileReferenceLiteral {
  public init(fileReferenceLiteralResourceName name: String) {
    self = Bundle.main.url(forResource: name, withExtension: nil)!
  }
}

public typealias _FileReferenceLiteralType = URL
