
var kDADiskMountOptionDefault: Int { get }
var kDADiskMountOptionWhole: Int { get }
typealias DADiskMountOptions = UInt32
var kDADiskRenameOptionDefault: Int { get }
typealias DADiskRenameOptions = UInt32
var kDADiskUnmountOptionDefault: Int { get }
var kDADiskUnmountOptionForce: Int { get }
var kDADiskUnmountOptionWhole: Int { get }
typealias DADiskUnmountOptions = UInt32
var kDADiskEjectOptionDefault: Int { get }
typealias DADiskEjectOptions = UInt32
var kDADiskClaimOptionDefault: Int { get }
typealias DADiskClaimOptions = UInt32
var kDADiskOptionDefault: Int { get }
typealias DADiskOptions = UInt32
var kDADiskDescriptionMatchMediaUnformatted: Unmanaged<CFDictionary>
var kDADiskDescriptionMatchMediaWhole: Unmanaged<CFDictionary>
var kDADiskDescriptionMatchVolumeMountable: Unmanaged<CFDictionary>
var kDADiskDescriptionMatchVolumeUnrecognized: Unmanaged<CFDictionary>
var kDADiskDescriptionWatchVolumeName: Unmanaged<CFArray>
var kDADiskDescriptionWatchVolumePath: Unmanaged<CFArray>
typealias DADiskAppearedCallback = @convention(c) (DADisk, UnsafeMutablePointer<Void>?) -> Void
func DARegisterDiskAppearedCallback(_ session: DASession, _ match: CFDictionary?, _ callback: DADiskAppearedCallback, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskDescriptionChangedCallback = @convention(c) (DADisk, CFArray, UnsafeMutablePointer<Void>?) -> Void
func DARegisterDiskDescriptionChangedCallback(_ session: DASession, _ match: CFDictionary?, _ watch: CFArray?, _ callback: DADiskDescriptionChangedCallback, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskDisappearedCallback = @convention(c) (DADisk, UnsafeMutablePointer<Void>?) -> Void
func DARegisterDiskDisappearedCallback(_ session: DASession, _ match: CFDictionary?, _ callback: DADiskDisappearedCallback, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskMountCallback = @convention(c) (DADisk, DADissenter?, UnsafeMutablePointer<Void>?) -> Void
func DADiskMount(_ disk: DADisk, _ path: CFURL?, _ options: DADiskMountOptions, _ callback: DADiskMountCallback?, _ context: UnsafeMutablePointer<Void>?)
func DADiskMountWithArguments(_ disk: DADisk, _ path: CFURL?, _ options: DADiskMountOptions, _ callback: DADiskMountCallback?, _ context: UnsafeMutablePointer<Void>?, _ arguments: UnsafeMutablePointer<Unmanaged<CFString>>!)
typealias DADiskMountApprovalCallback = @convention(c) (DADisk, UnsafeMutablePointer<Void>?) -> Unmanaged<DADissenter>?
func DARegisterDiskMountApprovalCallback(_ session: DASession, _ match: CFDictionary?, _ callback: DADiskMountApprovalCallback, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskRenameCallback = @convention(c) (DADisk, DADissenter?, UnsafeMutablePointer<Void>?) -> Void
func DADiskRename(_ disk: DADisk, _ name: CFString, _ options: DADiskRenameOptions, _ callback: DADiskRenameCallback?, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskUnmountCallback = @convention(c) (DADisk, DADissenter?, UnsafeMutablePointer<Void>?) -> Void
func DADiskUnmount(_ disk: DADisk, _ options: DADiskUnmountOptions, _ callback: DADiskUnmountCallback?, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskUnmountApprovalCallback = @convention(c) (DADisk, UnsafeMutablePointer<Void>?) -> Unmanaged<DADissenter>?
func DARegisterDiskUnmountApprovalCallback(_ session: DASession, _ match: CFDictionary?, _ callback: DADiskUnmountApprovalCallback, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskEjectCallback = @convention(c) (DADisk, DADissenter?, UnsafeMutablePointer<Void>?) -> Void
func DADiskEject(_ disk: DADisk, _ options: DADiskEjectOptions, _ callback: DADiskEjectCallback?, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskEjectApprovalCallback = @convention(c) (DADisk, UnsafeMutablePointer<Void>?) -> Unmanaged<DADissenter>?
func DARegisterDiskEjectApprovalCallback(_ session: DASession, _ match: CFDictionary?, _ callback: DADiskEjectApprovalCallback, _ context: UnsafeMutablePointer<Void>?)
typealias DADiskClaimCallback = @convention(c) (DADisk, DADissenter?, UnsafeMutablePointer<Void>?) -> Void
typealias DADiskClaimReleaseCallback = @convention(c) (DADisk, UnsafeMutablePointer<Void>?) -> Unmanaged<DADissenter>?
func DADiskClaim(_ disk: DADisk, _ options: DADiskClaimOptions, _ release: DADiskClaimReleaseCallback?, _ releaseContext: UnsafeMutablePointer<Void>?, _ callback: DADiskClaimCallback?, _ callbackContext: UnsafeMutablePointer<Void>?)
@discardableResult
func DADiskIsClaimed(_ disk: DADisk) -> Bool
func DADiskUnclaim(_ disk: DADisk)
typealias DADiskPeekCallback = @convention(c) (DADisk, UnsafeMutablePointer<Void>?) -> Void
func DARegisterDiskPeekCallback(_ session: DASession, _ match: CFDictionary?, _ order: CFIndex, _ callback: DADiskPeekCallback, _ context: UnsafeMutablePointer<Void>?)
@discardableResult
func DADiskGetOptions(_ disk: DADisk) -> DADiskOptions
@discardableResult
func DADiskSetOptions(_ disk: DADisk, _ options: DADiskOptions, _ value: Bool) -> DAReturn
func DAUnregisterCallback(_ session: DASession, _ callback: UnsafeMutablePointer<Void>, _ context: UnsafeMutablePointer<Void>?)
