
@available(OSX 10.9, *)
class SKReceiptRefreshRequest : SKRequest {
  init?(receiptProperties properties: [String : AnyObject])
  var receiptProperties: [String : AnyObject]? { get }
}
@available(OSX 10.7, *)
let SKReceiptPropertyIsExpired: String
@available(OSX 10.7, *)
let SKReceiptPropertyIsRevoked: String
@available(OSX 10.7, *)
let SKReceiptPropertyIsVolumePurchase: String
