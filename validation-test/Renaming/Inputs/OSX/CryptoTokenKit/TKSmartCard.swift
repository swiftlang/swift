
@available(OSX 10.10, *)
class TKSmartCardSlotManager : NSObject {
  @discardableResult
  class func defaultManager() -> Self?
  var slotNames: [String] { get }
  func getSlotWithName(_ name: String, reply reply: (TKSmartCardSlot?) -> Void)
}
@available(OSX 10.10, *)
enum TKSmartCardSlotState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case missing
  case empty
  case probing
  case muteCard
  case validCard
}
@available(OSX 10.11, *)
enum TKSmartCardPINCharset : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case numeric
  case alphanumeric
  case upperAlphanumeric
}
@available(OSX 10.11, *)
enum TKSmartCardPINEncoding : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case binary
  case ASCII
  case BCD
}
@available(OSX 10.11, *)
enum TKSmartCardPINJustification : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case left
  case right
}
@available(OSX 10.11, *)
struct TKSmartCardPINCompletion : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var maxLength: TKSmartCardPINCompletion { get }
  static var key: TKSmartCardPINCompletion { get }
  static var timeout: TKSmartCardPINCompletion { get }
}
@available(OSX 10.11, *)
struct TKSmartCardPINConfirmation : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var new: TKSmartCardPINConfirmation { get }
  static var current: TKSmartCardPINConfirmation { get }
}
@available(OSX 10.11, *)
class TKSmartCardPINFormat : NSObject {
  var charset: TKSmartCardPINCharset
  var encoding: TKSmartCardPINEncoding
  var minPINLength: Int
  var maxPINLength: Int
  var pinBlockByteLength: Int
  var pinJustification: TKSmartCardPINJustification
  var pinBitOffset: Int
  var pinLengthBitOffset: Int
  var pinLengthBitSize: Int
}
@available(OSX 10.11, *)
protocol TKSmartCardUserInteractionDelegate {
  optional func characterEntered(in interaction: TKSmartCardUserInteraction)
  optional func correctionKeyPressed(in interaction: TKSmartCardUserInteraction)
  optional func validationKeyPressed(in interaction: TKSmartCardUserInteraction)
  optional func invalidCharacterEntered(in interaction: TKSmartCardUserInteraction)
  optional func oldPINRequested(in interaction: TKSmartCardUserInteraction)
  optional func newPINRequested(in interaction: TKSmartCardUserInteraction)
  optional func newPINConfirmationRequested(in interaction: TKSmartCardUserInteraction)
}
@available(OSX 10.11, *)
class TKSmartCardUserInteraction : NSObject {
  weak var delegate: @sil_weak TKSmartCardUserInteractionDelegate?
  var initialTimeout: NSTimeInterval
  var interactionTimeout: NSTimeInterval
  func run(reply reply: (Bool, NSError?) -> Void)
  @discardableResult
  func cancel() -> Bool
}
@available(OSX 10.11, *)
class TKSmartCardUserInteractionForPINOperation : TKSmartCardUserInteraction {
  var pinCompletion: TKSmartCardPINCompletion
  var pinMessageIndices: [NSNumber]?
  var locale: NSLocale!
  var resultSW: UInt16
  var resultData: NSData?
}
@available(OSX 10.11, *)
class TKSmartCardUserInteractionForSecurePINVerification : TKSmartCardUserInteractionForPINOperation {
}
@available(OSX 10.11, *)
class TKSmartCardUserInteractionForSecurePINChange : TKSmartCardUserInteractionForPINOperation {
  var pinConfirmation: TKSmartCardPINConfirmation
}
@available(OSX 10.10, *)
class TKSmartCardSlot : NSObject {
  var state: TKSmartCardSlotState { get }
  var atr: TKSmartCardATR? { get }
  var name: String { get }
  var maxInputLength: Int { get }
  var maxOutputLength: Int { get }
  @discardableResult
  func makeSmartCard() -> TKSmartCard?
}
@available(OSX 10.10, *)
class TKSmartCard : NSObject {
  var slot: TKSmartCardSlot { get }
  var valid: Bool { get }
  var allowedProtocols: TKSmartCardProtocol
  var currentProtocol: TKSmartCardProtocol { get }
  var sensitive: Bool
  var context: AnyObject?
  func beginSession(reply reply: (Bool, NSError?) -> Void)
  func transmitRequest(_ request: NSData, reply reply: (NSData?, NSError?) -> Void)
  func endSession()
  @available(OSX 10.11, *)
  @discardableResult
  func userInteractionForSecurePINVerification(with PINFormat: TKSmartCardPINFormat, apdu APDU: NSData, pinByteOffset PINByteOffset: Int) -> TKSmartCardUserInteractionForSecurePINVerification?
  @available(OSX 10.11, *)
  @discardableResult
  func userInteractionForSecurePINChange(with PINFormat: TKSmartCardPINFormat, apdu APDU: NSData, currentPINByteOffset currentPINByteOffset: Int, newPINByteOffset newPINByteOffset: Int) -> TKSmartCardUserInteractionForSecurePINChange?
}
extension TKSmartCard {
  var cla: UInt8
  var useExtendedLength: Bool
  func sendIns(_ ins: UInt8, p1 p1: UInt8, p2 p2: UInt8, data requestData: NSData?, le le: NSNumber?, reply reply: (NSData?, UInt16, NSError?) -> Void)
}
