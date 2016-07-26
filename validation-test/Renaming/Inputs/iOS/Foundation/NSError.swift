
let NSCocoaErrorDomain: String
let NSPOSIXErrorDomain: String
let NSOSStatusErrorDomain: String
let NSMachErrorDomain: String
let NSUnderlyingErrorKey: String
let NSLocalizedDescriptionKey: String
let NSLocalizedFailureReasonErrorKey: String
let NSLocalizedRecoverySuggestionErrorKey: String
let NSLocalizedRecoveryOptionsErrorKey: String
let NSRecoveryAttempterErrorKey: String
let NSHelpAnchorErrorKey: String
let NSStringEncodingErrorKey: String
let NSURLErrorKey: String
let NSFilePathErrorKey: String
class NSError : NSObject, NSCopying, NSSecureCoding {
  init(domain domain: String, code code: Int, userInfo dict: [NSObject : AnyObject]? = [:])
  var domain: String { get }
  var code: Int { get }
  var userInfo: [NSObject : AnyObject] { get }
  var localizedDescription: String { get }
  var localizedFailureReason: String? { get }
  var localizedRecoverySuggestion: String? { get }
  var localizedRecoveryOptions: [String]? { get }
  var recoveryAttempter: AnyObject? { get }
  var helpAnchor: String? { get }
  @available(iOS 9.0, *)
  class func setUserInfoValueProviderForDomain(_ errorDomain: String, provider provider: ((NSError, String) -> AnyObject?)? = nil)
  @available(iOS 9.0, *)
  @discardableResult
  class func userInfoValueProvider(forDomain errorDomain: String) -> ((NSError, String) -> AnyObject?)?
}

extension NSError : ErrorProtocol {
}
extension NSObject {
  class func attemptRecovery(fromError error: NSError, optionIndex recoveryOptionIndex: Int, delegate delegate: AnyObject?, didRecoverSelector didRecoverSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  func attemptRecovery(fromError error: NSError, optionIndex recoveryOptionIndex: Int, delegate delegate: AnyObject?, didRecoverSelector didRecoverSelector: Selector?, contextInfo contextInfo: UnsafeMutablePointer<Void>?)
  @discardableResult
  class func attemptRecovery(fromError error: NSError, optionIndex recoveryOptionIndex: Int) -> Bool
  @discardableResult
  func attemptRecovery(fromError error: NSError, optionIndex recoveryOptionIndex: Int) -> Bool
}
