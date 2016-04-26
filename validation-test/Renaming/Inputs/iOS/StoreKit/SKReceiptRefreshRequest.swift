
@available(iOS 7.0, *)
class SKReceiptRefreshRequest : SKRequest {
  @available(iOS 7.0, *)
  init(receiptProperties properties: [String : AnyObject]?)
  @available(iOS 7.0, *)
  var receiptProperties: [String : AnyObject]? { get }
}
@available(iOS 7.1, *)
func SKTerminateForInvalidReceipt()
@available(iOS 7.0, *)
let SKReceiptPropertyIsExpired: String
@available(iOS 7.0, *)
let SKReceiptPropertyIsRevoked: String
@available(iOS 7.0, *)
let SKReceiptPropertyIsVolumePurchase: String
