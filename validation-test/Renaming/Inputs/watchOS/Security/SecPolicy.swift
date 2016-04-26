
@available(watchOS 2.0, *)
let kSecPolicyAppleX509Basic: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleSSL: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleSMIME: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleEAP: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleIPsec: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleCodeSigning: CFString
@available(watchOS 2.0, *)
let kSecPolicyMacAppStoreReceipt: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleIDValidation: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleTimeStamping: CFString
@available(watchOS 2.0, *)
let kSecPolicyAppleRevocation: CFString
@available(watchOS 2.0, *)
let kSecPolicyApplePayIssuerEncryption: CFString
@available(watchOS 2.0, *)
let kSecPolicyOid: CFString
@available(watchOS 2.0, *)
let kSecPolicyName: CFString
@available(watchOS 2.0, *)
let kSecPolicyClient: CFString
@available(watchOS 2.0, *)
let kSecPolicyRevocationFlags: CFString
@available(watchOS 2.0, *)
@discardableResult
func SecPolicyGetTypeID() -> CFTypeID
@available(watchOS 2.0, *)
@discardableResult
func SecPolicyCopyProperties(_ policyRef: SecPolicy) -> CFDictionary
@available(watchOS 2.0, *)
@discardableResult
func SecPolicyCreateBasicX509() -> SecPolicy
@available(watchOS 2.0, *)
@discardableResult
func SecPolicyCreateSSL(_ server: Bool, _ hostname: CFString?) -> SecPolicy
var kSecRevocationOCSPMethod: CFOptionFlags { get }
var kSecRevocationCRLMethod: CFOptionFlags { get }
var kSecRevocationPreferCRL: CFOptionFlags { get }
var kSecRevocationRequirePositiveResponse: CFOptionFlags { get }
var kSecRevocationNetworkAccessDisabled: CFOptionFlags { get }
var kSecRevocationUseAnyAvailableMethod: CFOptionFlags { get }
@available(watchOS 2.0, *)
@discardableResult
func SecPolicyCreateRevocation(_ revocationFlags: CFOptionFlags) -> SecPolicy
@available(watchOS 2.0, *)
@discardableResult
func SecPolicyCreateWithProperties(_ policyIdentifier: CFTypeRef, _ properties: CFDictionary?) -> SecPolicy
