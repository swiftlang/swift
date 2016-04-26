
@available(OSX, introduced: 10.6, deprecated: 10.10)
let kSMErrorDomainIPC: CFString!
@available(OSX, introduced: 10.6, deprecated: 10.10)
let kSMErrorDomainFramework: CFString!
@available(OSX, introduced: 10.6, deprecated: 10.10)
let kSMErrorDomainLaunchd: CFString!
var kSMErrorInternalFailure: Int { get }
var kSMErrorInvalidSignature: Int { get }
var kSMErrorAuthorizationFailure: Int { get }
var kSMErrorToolNotValid: Int { get }
var kSMErrorJobNotFound: Int { get }
var kSMErrorServiceUnavailable: Int { get }
var kSMErrorJobPlistNotFound: Int { get }
var kSMErrorJobMustBeEnabled: Int { get }
var kSMErrorInvalidPlist: Int { get }
@available(OSX 10.6, *)
@discardableResult
func SMLoginItemSetEnabled(_ identifier: CFString, _ enabled: Bool) -> Bool
var kSMRightBlessPrivilegedHelper: String { get }
var kSMRightModifySystemDaemons: String { get }
@available(OSX 10.6, *)
let kSMDomainSystemLaunchd: CFString!
@available(OSX 10.6, *)
let kSMDomainUserLaunchd: CFString!
@available(OSX, introduced: 10.6, deprecated: 10.10)
@discardableResult
func SMJobCopyDictionary(_ domain: CFString!, _ jobLabel: CFString) -> Unmanaged<CFDictionary>!
@available(OSX, introduced: 10.6, deprecated: 10.10)
@discardableResult
func SMCopyAllJobDictionaries(_ domain: CFString!) -> Unmanaged<CFArray>!
@available(OSX, introduced: 10.6, deprecated: 10.10)
@discardableResult
func SMJobSubmit(_ domain: CFString!, _ job: CFDictionary, _ auth: AuthorizationRef!, _ outError: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(OSX, introduced: 10.6, deprecated: 10.10)
@discardableResult
func SMJobRemove(_ domain: CFString!, _ jobLabel: CFString, _ auth: AuthorizationRef!, _ wait: Bool, _ outError: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
@available(OSX 10.6, *)
@discardableResult
func SMJobBless(_ domain: CFString!, _ executableLabel: CFString, _ auth: AuthorizationRef!, _ outError: UnsafeMutablePointer<Unmanaged<CFError>?>!) -> Bool
