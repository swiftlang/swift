
@available(watchOS 2.0, *)
enum PKPassType : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case barcode
  case payment
  case any
}
class PKPass : PKObject {
  init(data data: NSData, error error: NSErrorPointer)
  @available(watchOS 2.0, *)
  var passType: PKPassType { get }
  @available(watchOS 2.0, *)
  unowned(unsafe) var paymentPass: @sil_unmanaged PKPaymentPass? { get }
  var serialNumber: String { get }
  var passTypeIdentifier: String { get }
  @NSCopying var webServiceURL: NSURL? { get }
  var authenticationToken: String? { get }
  var localizedName: String { get }
  var localizedDescription: String { get }
  var organizationName: String { get }
  @NSCopying var relevantDate: NSDate? { get }
  @available(watchOS 2.0, *)
  var userInfo: [NSObject : AnyObject]? { get }
  @NSCopying var passURL: NSURL { get }
  @available(watchOS 2.0, *)
  var isRemotePass: Bool { get }
  @available(watchOS 2.0, *)
  var deviceName: String { get }
  @discardableResult
  func localizedValue(forFieldKey key: String) -> AnyObject?
}
