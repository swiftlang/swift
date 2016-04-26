
@available(OSX 10.9, *)
@discardableResult
func CWKeychainFindWiFiPassword(_ domain: CWKeychainDomain, _ ssid: NSData, _ password: AutoreleasingUnsafeMutablePointer<NSString?>?) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CWKeychainSetWiFiPassword(_ domain: CWKeychainDomain, _ ssid: NSData, _ password: String) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CWKeychainDeleteWiFiPassword(_ domain: CWKeychainDomain, _ ssid: NSData) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CWKeychainFindWiFiEAPUsernameAndPassword(_ domain: CWKeychainDomain, _ ssid: NSData, _ username: AutoreleasingUnsafeMutablePointer<NSString?>?, _ password: AutoreleasingUnsafeMutablePointer<NSString?>?) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CWKeychainSetWiFiEAPUsernameAndPassword(_ domain: CWKeychainDomain, _ ssid: NSData, _ username: String?, _ password: String?) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CWKeychainDeleteWiFiEAPUsernameAndPassword(_ domain: CWKeychainDomain, _ ssid: NSData) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CWKeychainCopyWiFiEAPIdentity(_ domain: CWKeychainDomain, _ ssid: NSData, _ identity: UnsafeMutablePointer<Unmanaged<SecIdentity>?>?) -> OSStatus
@available(OSX 10.9, *)
@discardableResult
func CWKeychainSetWiFiEAPIdentity(_ domain: CWKeychainDomain, _ ssid: NSData, _ identity: SecIdentity?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CWKeychainCopyEAPIdentityList(_ list: UnsafeMutablePointer<Unmanaged<CFArray>?>?) -> OSStatus
@available(OSX 10.7, *)
@discardableResult
func CWMergeNetworks(_ networks: Set<CWNetwork>) -> Set<CWNetwork>
