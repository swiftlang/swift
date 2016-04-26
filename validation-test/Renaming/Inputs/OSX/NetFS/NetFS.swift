
typealias netfsError = Int32
var kNetFSAuthenticationInfoKey: String { get }
var kNetFSServerDisplayNameKey: String { get }
var kNetFSSupportsChangePasswordKey: String { get }
var kNetFSSupportsGuestKey: String { get }
var kNetFSSupportsKerberosKey: String { get }
var kNetFSGuestOnlyKey: String { get }
var kNetFSNoMountAuthenticationKey: String { get }
var kNetFSConnectedWithAuthenticationInfoKey: String { get }
var kNetFSConnectedAsUserKey: String { get }
var kNetFSConnectedAsGuestKey: String { get }
var kNetFSConnectedMultiUserKey: String { get }
var kNetFSMechTypesSupportedKey: String { get }
var kNAUIOptionKey: String { get }
var kNAUIOptionNoUI: String { get }
var kNAUIOptionAllowUI: String { get }
var kNAUIOptionForceUI: String { get }
var kNetFSSchemeKey: String { get }
var kNetFSHostKey: String { get }
var kNetFSAlternatePortKey: String { get }
var kNetFSAuthorityParamsKey: String { get }
var kNetFSUserNameKey: String { get }
var kNetFSPathKey: String { get }
var kNetFSNoUserPreferencesKey: String { get }
var kNetFSForceNewSessionKey: String { get }
var kNetFSUseAuthenticationInfoKey: String { get }
var kNetFSUseGuestKey: String { get }
var kNetFSChangePasswordKey: String { get }
var kNetFSAllowLoopbackKey: String { get }
var kNetFSUseKerberosKey: String { get }
var kNetFSMountedWithAuthenticationInfoKey: String { get }
var kNetFSMountedByUserKey: String { get }
var kNetFSMountedByGuestKey: String { get }
var kNetFSMountedMultiUserKey: String { get }
var kNetFSMountedByKerberosKey: String { get }
var ENETFSPWDNEEDSCHANGE: Int32 { get }
var ENETFSPWDPOLICY: Int32 { get }
var ENETFSACCOUNTRESTRICTED: Int32 { get }
var ENETFSNOSHARESAVAIL: Int32 { get }
var ENETFSNOAUTHMECHSUPP: Int32 { get }
var ENETFSNOPROTOVERSSUPP: Int32 { get }
var kNetFSGetAccessRightsKey: String { get }
var kNetFSAlreadyMountedKey: String { get }
var kNetFSMountPathKey: String { get }
var kNetFSHasPasswordKey: String { get }
var kNetFSIsHiddenKey: String { get }
var kNetFSPrinterShareKey: String { get }
var kNetFSAccessRightsKey: String { get }
var kNetFSDisplayNameKey: String { get }
var kNetFSPasswordKey: String { get }
var kNetFSSoftMountKey: String { get }
var kNetFSMountFlagsKey: String { get }
var kNetFSAllowSubMountsKey: String { get }
var kNetFSMountAtMountDirKey: String { get }
var kNetFSMountedURLKey: String { get }
typealias AsyncRequestID = UnsafeMutablePointer<Void>
@available(OSX 10.8, *)
@discardableResult
func NetFSMountURLSync(_ url: CFURL!, _ mountpath: CFURL!, _ user: CFString!, _ passwd: CFString!, _ open_options: CFMutableDictionary!, _ mount_options: CFMutableDictionary!, _ mountpoints: UnsafeMutablePointer<Unmanaged<CFArray>?>!) -> Int32
typealias NetFSMountURLBlock = (Int32, AsyncRequestID!, CFArray!) -> Void
@available(OSX 10.8, *)
@discardableResult
func NetFSMountURLAsync(_ url: CFURL!, _ mountpath: CFURL!, _ user: CFString!, _ passwd: CFString!, _ open_options: CFMutableDictionary!, _ mount_options: CFMutableDictionary!, _ requestID: UnsafeMutablePointer<AsyncRequestID?>!, _ dispatchq: dispatch_queue_t!, _ mount_report: NetFSMountURLBlock!) -> Int32
@available(OSX 10.8, *)
@discardableResult
func NetFSMountURLCancel(_ requestID: AsyncRequestID!) -> Int32
@discardableResult
func NetFSMountURLProbe(_ hostname: CFString!) -> Unmanaged<CFString>!
@discardableResult
func NetFSCopyURLForRemountingVolume(_ localPathURL: CFURL!) -> Unmanaged<CFURL>!
