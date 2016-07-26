
@available(tvOS 3.0, *)
enum SKPaymentTransactionState : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case purchasing
  case purchased
  case failed
  case restored
  @available(tvOS 8.0, *)
  case deferred
}
@available(tvOS 3.0, *)
class SKPaymentTransaction : NSObject {
  @available(tvOS 3.0, *)
  var error: NSError? { get }
  @available(tvOS 3.0, *)
  var original: SKPaymentTransaction? { get }
  @available(tvOS 3.0, *)
  var payment: SKPayment { get }
  @available(tvOS 6.0, *)
  var downloads: [SKDownload] { get }
  @available(tvOS 3.0, *)
  var transactionDate: NSDate? { get }
  @available(tvOS 3.0, *)
  var transactionIdentifier: String? { get }
  @available(tvOS 3.0, *)
  var transactionState: SKPaymentTransactionState { get }
}
