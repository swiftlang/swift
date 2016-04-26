
@available(OSX 10.7, *)
let kSecPolicyAppleX509Basic: CFString
@available(OSX 10.7, *)
let kSecPolicyAppleSSL: CFString
@available(OSX 10.7, *)
let kSecPolicyAppleSMIME: CFString
@available(OSX 10.7, *)
let kSecPolicyAppleEAP: CFString
@available(OSX 10.7, *)
let kSecPolicyAppleIPsec: CFString
@available(OSX 10.7, *)
let kSecPolicyApplePKINITClient: CFString
@available(OSX 10.7, *)
let kSecPolicyApplePKINITServer: CFString
@available(OSX 10.7, *)
let kSecPolicyAppleCodeSigning: CFString
@available(OSX 10.7, *)
let kSecPolicyMacAppStoreReceipt: CFString
@available(OSX 10.7, *)
let kSecPolicyAppleIDValidation: CFString
@available(OSX 10.8, *)
let kSecPolicyAppleTimeStamping: CFString
@available(OSX 10.9, *)
let kSecPolicyAppleRevocation: CFString
@available(OSX 10.9, *)
let kSecPolicyApplePassbookSigning: CFString
@available(OSX 10.11, *)
let kSecPolicyApplePayIssuerEncryption: CFString
@available(OSX 10.7, *)
let kSecPolicyOid: CFString
@available(OSX 10.7, *)
let kSecPolicyName: CFString
@available(OSX 10.7, *)
let kSecPolicyClient: CFString
@available(OSX 10.9, *)
let kSecPolicyRevocationFlags: CFString
@available(OSX 10.9, *)
let kSecPolicyTeamIdentifier: CFString
@available(OSX 10.3, *)
@discardableResult
func SecPolicyGetTypeID() -> CFTypeID
@available(OSX 10.7, *)
@discardableResult
func SecPolicyCopyProperties(_ policyRef: SecPolicy) -> CFDictionary
@available(OSX 10.6, *)
@discardableResult
func SecPolicyCreateBasicX509() -> SecPolicy
@available(OSX 10.6, *)
@discardableResult
func SecPolicyCreateSSL(_ server: Bool, _ hostname: CFString?) -> SecPolicy
var kSecRevocationOCSPMethod: Int { get }
var kSecRevocationCRLMethod: Int { get }
var kSecRevocationPreferCRL: Int { get }
var kSecRevocationRequirePositiveResponse: Int { get }
var kSecRevocationNetworkAccessDisabled: Int { get }
var kSecRevocationUseAnyAvailableMethod: Int { get }
@available(OSX 10.9, *)
@discardableResult
func SecPolicyCreateRevocation(_ revocationFlags: CFOptionFlags) -> SecPolicy
@available(OSX 10.9, *)
@discardableResult
func SecPolicyCreateWithProperties(_ policyIdentifier: CFTypeRef, _ properties: CFDictionary?) -> SecPolicy?
@available(OSX 10.7, *)
let kSecPolicyKU_DigitalSignature: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_NonRepudiation: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_KeyEncipherment: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_DataEncipherment: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_KeyAgreement: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_KeyCertSign: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_CRLSign: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_EncipherOnly: CFString
@available(OSX 10.7, *)
let kSecPolicyKU_DecipherOnly: CFString
