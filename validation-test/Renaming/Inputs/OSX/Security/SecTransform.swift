
let kSecTransformErrorDomain: CFString
let kSecTransformPreviousErrorKey: CFString
let kSecTransformAbortOriginatorKey: CFString
var kSecTransformErrorAttributeNotFound: CFIndex { get }
var kSecTransformErrorInvalidOperation: CFIndex { get }
var kSecTransformErrorNotInitializedCorrectly: CFIndex { get }
var kSecTransformErrorMoreThanOneOutput: CFIndex { get }
var kSecTransformErrorInvalidInputDictionary: CFIndex { get }
var kSecTransformErrorInvalidAlgorithm: CFIndex { get }
var kSecTransformErrorInvalidLength: CFIndex { get }
var kSecTransformErrorInvalidType: CFIndex { get }
var kSecTransformErrorInvalidInput: CFIndex { get }
var kSecTransformErrorNameAlreadyRegistered: CFIndex { get }
var kSecTransformErrorUnsupportedAttribute: CFIndex { get }
var kSecTransformOperationNotSupportedOnGroup: CFIndex { get }
var kSecTransformErrorMissingParameter: CFIndex { get }
var kSecTransformErrorInvalidConnection: CFIndex { get }
var kSecTransformTransformIsExecuting: CFIndex { get }
var kSecTransformInvalidOverride: CFIndex { get }
var kSecTransformTransformIsNotRegistered: CFIndex { get }
var kSecTransformErrorAbortInProgress: CFIndex { get }
var kSecTransformErrorAborted: CFIndex { get }
var kSecTransformInvalidArgument: CFIndex { get }
typealias SecTransform = CFTypeRef
typealias SecGroupTransform = CFTypeRef
@discardableResult
func SecTransformGetTypeID() -> CFTypeID
@discardableResult
func SecGroupTransformGetTypeID() -> CFTypeID
@available(OSX 10.7, *)
let kSecTransformInputAttributeName: CFString
@available(OSX 10.7, *)
let kSecTransformOutputAttributeName: CFString
@available(OSX 10.7, *)
let kSecTransformDebugAttributeName: CFString
@available(OSX 10.7, *)
let kSecTransformTransformName: CFString
@available(OSX 10.7, *)
let kSecTransformAbortAttributeName: CFString
@available(OSX 10.7, *)
@discardableResult
func SecTransformCreateFromExternalRepresentation(_ dictionary: CFDictionary, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecTransform?
@available(OSX 10.7, *)
@discardableResult
func SecTransformCopyExternalRepresentation(_ transformRef: SecTransform) -> CFDictionary
@discardableResult
func SecTransformCreateGroupTransform() -> SecGroupTransform
@available(OSX 10.7, *)
@discardableResult
func SecTransformConnectTransforms(_ sourceTransformRef: SecTransform, _ sourceAttributeName: CFString, _ destinationTransformRef: SecTransform, _ destinationAttributeName: CFString, _ group: SecGroupTransform, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> SecGroupTransform?
@available(OSX 10.7, *)
@discardableResult
func SecTransformSetAttribute(_ transformRef: SecTransform, _ key: CFString, _ value: CFTypeRef, _ error: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> Bool
@available(OSX 10.7, *)
@discardableResult
func SecTransformGetAttribute(_ transformRef: SecTransform, _ key: CFString) -> CFTypeRef?
@available(OSX 10.7, *)
@discardableResult
func SecTransformFindByName(_ transform: SecGroupTransform, _ name: CFString) -> SecTransform?
@available(OSX 10.7, *)
@discardableResult
func SecTransformExecute(_ transformRef: SecTransform, _ errorRef: UnsafeMutablePointer<Unmanaged<CFError>?>?) -> CFTypeRef
typealias SecMessageBlock = (CFTypeRef?, CFError?, Bool) -> Void
@available(OSX 10.7, *)
func SecTransformExecuteAsync(_ transformRef: SecTransform, _ deliveryQueue: dispatch_queue_t, _ deliveryBlock: SecMessageBlock)
