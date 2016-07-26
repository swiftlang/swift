
@available(OSX 10.11, *)
struct NSAppleEventSendOptions : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var noReply: NSAppleEventSendOptions { get }
  static var queueReply: NSAppleEventSendOptions { get }
  static var waitForReply: NSAppleEventSendOptions { get }
  static var neverInteract: NSAppleEventSendOptions { get }
  static var canInteract: NSAppleEventSendOptions { get }
  static var alwaysInteract: NSAppleEventSendOptions { get }
  static var canSwitchLayer: NSAppleEventSendOptions { get }
  static var dontRecord: NSAppleEventSendOptions { get }
  static var dontExecute: NSAppleEventSendOptions { get }
  static var dontAnnotate: NSAppleEventSendOptions { get }
  static var defaultOptions: NSAppleEventSendOptions { get }
}
class NSAppleEventDescriptor : NSObject, NSCopying, NSSecureCoding {
  @discardableResult
  class func null() -> NSAppleEventDescriptor
  /*not inherited*/ init(boolean boolean: Bool)
  /*not inherited*/ init(enumCode enumerator: OSType)
  /*not inherited*/ init(int32 signedInt: Int32)
  @available(OSX 10.11, *)
  /*not inherited*/ init(double doubleValue: Double)
  /*not inherited*/ init(typeCode typeCode: OSType)
  /*not inherited*/ init(string string: String)
  @available(OSX 10.11, *)
  /*not inherited*/ init(date date: NSDate)
  @available(OSX 10.11, *)
  /*not inherited*/ init(fileURL fileURL: NSURL)
  @discardableResult
  class func appleEvent(withEventClass eventClass: AEEventClass, eventID eventID: AEEventID, targetDescriptor targetDescriptor: NSAppleEventDescriptor?, returnID returnID: AEReturnID, transactionID transactionID: AETransactionID) -> NSAppleEventDescriptor
  @discardableResult
  class func list() -> NSAppleEventDescriptor
  @discardableResult
  class func record() -> NSAppleEventDescriptor
  @available(OSX 10.11, *)
  @discardableResult
  class func currentProcess() -> NSAppleEventDescriptor
  @available(OSX 10.11, *)
  /*not inherited*/ init(processIdentifier processIdentifier: pid_t)
  @available(OSX 10.11, *)
  /*not inherited*/ init(bundleIdentifier bundleIdentifier: String)
  @available(OSX 10.11, *)
  /*not inherited*/ init(applicationURL applicationURL: NSURL)
  init(aeDescNoCopy aeDesc: UnsafePointer<AEDesc>)
  convenience init?(descriptorType descriptorType: DescType, bytes bytes: UnsafePointer<Void>?, length byteCount: Int)
  convenience init?(descriptorType descriptorType: DescType, data data: NSData?)
  convenience init(eventClass eventClass: AEEventClass, eventID eventID: AEEventID, targetDescriptor targetDescriptor: NSAppleEventDescriptor?, returnID returnID: AEReturnID, transactionID transactionID: AETransactionID)
  convenience init(listDescriptor listDescriptor: ())
  convenience init(recordDescriptor recordDescriptor: ())
  var aeDesc: UnsafePointer<AEDesc>? { get }
  var descriptorType: DescType { get }
  @NSCopying var data: NSData { get }
  var booleanValue: Bool { get }
  var enumCodeValue: OSType { get }
  var int32Value: Int32 { get }
  @available(OSX 10.11, *)
  var doubleValue: Double { get }
  var typeCodeValue: OSType { get }
  var stringValue: String? { get }
  @available(OSX 10.11, *)
  @NSCopying var dateValue: NSDate? { get }
  @available(OSX 10.11, *)
  @NSCopying var fileURLValue: NSURL? { get }
  var eventClass: AEEventClass { get }
  var eventID: AEEventID { get }
  var returnID: AEReturnID { get }
  var transactionID: AETransactionID { get }
  func setParamDescriptor(_ descriptor: NSAppleEventDescriptor, forKeyword keyword: AEKeyword)
  @discardableResult
  func paramDescriptor(forKeyword keyword: AEKeyword) -> NSAppleEventDescriptor?
  func removeParamDescriptor(withKeyword keyword: AEKeyword)
  func setAttributeDescriptor(_ descriptor: NSAppleEventDescriptor, forKeyword keyword: AEKeyword)
  @discardableResult
  func attributeDescriptor(forKeyword keyword: AEKeyword) -> NSAppleEventDescriptor?
  @available(OSX 10.11, *)
  @discardableResult
  func sendEvent(_ sendOptions: NSAppleEventSendOptions = [], timeout timeoutInSeconds: NSTimeInterval) throws -> NSAppleEventDescriptor
  @available(OSX 10.11, *)
  var isRecordDescriptor: Bool { get }
  var numberOfItems: Int { get }
  func insert(_ descriptor: NSAppleEventDescriptor, at index: Int)
  @discardableResult
  func atIndex(_ index: Int) -> NSAppleEventDescriptor?
  func remove(at index: Int)
  func setDescriptor(_ descriptor: NSAppleEventDescriptor, forKeyword keyword: AEKeyword)
  @discardableResult
  func forKeyword(_ keyword: AEKeyword) -> NSAppleEventDescriptor?
  func remove(withKeyword keyword: AEKeyword)
  @discardableResult
  func keywordForDescriptor(at index: Int) -> AEKeyword
  @discardableResult
  func coerce(toDescriptorType descriptorType: DescType) -> NSAppleEventDescriptor?
}
