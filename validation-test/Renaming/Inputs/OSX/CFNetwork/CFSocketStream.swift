
@available(OSX 10.9, *)
let kCFStreamPropertySSLContext: CFString
@available(OSX 10.5, *)
let kCFStreamPropertySSLPeerTrust: CFString
@available(OSX 10.4, *)
let kCFStreamSSLValidatesCertificateChain: CFString
@available(OSX 10.4, *)
let kCFStreamPropertySSLSettings: CFString
@available(OSX 10.4, *)
let kCFStreamSSLLevel: CFString
@available(OSX 10.4, *)
let kCFStreamSSLPeerName: CFString
@available(OSX 10.4, *)
let kCFStreamSSLCertificates: CFString
@available(OSX 10.4, *)
let kCFStreamSSLIsServer: CFString
@available(OSX 10.7, *)
let kCFStreamNetworkServiceType: CFString
@available(OSX 10.7, *)
let kCFStreamNetworkServiceTypeVideo: CFString
@available(OSX 10.7, *)
let kCFStreamNetworkServiceTypeVoice: CFString
@available(OSX 10.7, *)
let kCFStreamNetworkServiceTypeBackground: CFString
@available(OSX, introduced: 10.7, deprecated: 10.11, message: "use PushKit for VoIP control purposes")
let kCFStreamNetworkServiceTypeVoIP: CFString
@available(OSX 10.8, *)
let kCFStreamPropertyNoCellular: CFString
@available(OSX 10.8, *)
let kCFStreamPropertyConnectionIsCellular: CFString
@available(OSX 10.5, *)
let kCFStreamErrorDomainWinSock: CFIndex
@available(OSX 10.0, *)
let kCFStreamErrorDomainSOCKS: Int32
@discardableResult
func CFSocketStreamSOCKSGetErrorSubdomain(_ error: UnsafePointer<CFStreamError>) -> Int32
@discardableResult
func CFSocketStreamSOCKSGetError(_ error: UnsafePointer<CFStreamError>) -> Int32
var kCFStreamErrorSOCKSSubDomainNone: Int { get }
var kCFStreamErrorSOCKSSubDomainVersionCode: Int { get }
var kCFStreamErrorSOCKS4SubDomainResponse: Int { get }
var kCFStreamErrorSOCKS5SubDomainUserPass: Int { get }
var kCFStreamErrorSOCKS5SubDomainMethod: Int { get }
var kCFStreamErrorSOCKS5SubDomainResponse: Int { get }
var kCFStreamErrorSOCKS5BadResponseAddr: Int { get }
var kCFStreamErrorSOCKS5BadState: Int { get }
var kCFStreamErrorSOCKSUnknownClientVersion: Int { get }
var kCFStreamErrorSOCKS4RequestFailed: Int { get }
var kCFStreamErrorSOCKS4IdentdFailed: Int { get }
var kCFStreamErrorSOCKS4IdConflict: Int { get }
var kSOCKS5NoAcceptableMethod: Int { get }
@available(OSX 10.2, *)
let kCFStreamPropertySOCKSProxy: CFString
@available(OSX 10.2, *)
let kCFStreamPropertySOCKSProxyHost: CFString
@available(OSX 10.2, *)
let kCFStreamPropertySOCKSProxyPort: CFString
@available(OSX 10.2, *)
let kCFStreamPropertySOCKSVersion: CFString
@available(OSX 10.2, *)
let kCFStreamSocketSOCKSVersion4: CFString
@available(OSX 10.2, *)
let kCFStreamSocketSOCKSVersion5: CFString
@available(OSX 10.2, *)
let kCFStreamPropertySOCKSUser: CFString
@available(OSX 10.2, *)
let kCFStreamPropertySOCKSPassword: CFString
@available(OSX 10.4, *)
let kCFStreamPropertyProxyLocalBypass: CFString
@available(OSX 10.2, *)
let kCFStreamErrorDomainSSL: Int32
@available(OSX 10.2, *)
let kCFStreamPropertySocketSecurityLevel: CFString
@available(OSX 10.2, *)
let kCFStreamSocketSecurityLevelNone: CFString
@available(OSX 10.2, *)
let kCFStreamSocketSecurityLevelSSLv2: CFString
@available(OSX 10.2, *)
let kCFStreamSocketSecurityLevelSSLv3: CFString
@available(OSX 10.2, *)
let kCFStreamSocketSecurityLevelTLSv1: CFString
@available(OSX 10.2, *)
let kCFStreamSocketSecurityLevelNegotiatedSSL: CFString
@available(OSX 10.2, *)
let kCFStreamPropertyShouldCloseNativeSocket: CFString
@available(OSX 10.3, *)
let kCFStreamPropertySocketRemoteHost: CFString
@available(OSX 10.3, *)
let kCFStreamPropertySocketRemoteNetService: CFString
@available(OSX 10.11, *)
let kCFStreamPropertySocketExtendedBackgroundIdleMode: CFString
@available(OSX 10.3, *)
func CFStreamCreatePairWithSocketToCFHost(_ alloc: CFAllocator?, _ host: CFHost, _ port: Int32, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>?, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>?)
@available(OSX 10.3, *)
func CFStreamCreatePairWithSocketToNetService(_ alloc: CFAllocator?, _ service: CFNetService, _ readStream: UnsafeMutablePointer<Unmanaged<CFReadStream>?>?, _ writeStream: UnsafeMutablePointer<Unmanaged<CFWriteStream>?>?)
