
let NSGenericException: String
let NSRangeException: String
let NSInvalidArgumentException: String
let NSInternalInconsistencyException: String
let NSMallocException: String
let NSObjectInaccessibleException: String
let NSObjectNotAvailableException: String
let NSDestinationInvalidException: String
let NSPortTimeoutException: String
let NSInvalidSendPortException: String
let NSInvalidReceivePortException: String
let NSPortSendException: String
let NSPortReceiveException: String
let NSOldStyleException: String
class NSException : NSObject, NSCopying, NSCoding {
  init(name aName: String, reason aReason: String?, userInfo aUserInfo: [NSObject : AnyObject]? = [:])
  var name: String { get }
  var reason: String? { get }
  var userInfo: [NSObject : AnyObject]? { get }
  @available(tvOS 2.0, *)
  var callStackReturnAddresses: [NSNumber] { get }
  @available(tvOS 4.0, *)
  var callStackSymbols: [String] { get }
  func raise()
}
extension NSException {
  class func raise(_ name: String, format format: String, arguments argList: CVaListPointer)
}
typealias NSUncaughtExceptionHandler = (NSException) -> Void
@discardableResult
func NSGetUncaughtExceptionHandler() -> (@convention(c) (NSException) -> Void)?
func NSSetUncaughtExceptionHandler(_ _: (@convention(c) (NSException) -> Void)?)
@available(tvOS 4.0, *)
let NSAssertionHandlerKey: String
class NSAssertionHandler : NSObject {
  @discardableResult
  class func current() -> NSAssertionHandler
}
