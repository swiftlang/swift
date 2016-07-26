
var SKPaymentTransactionStatePurchasing: Int { get }
var SKPaymentTransactionStatePurchased: Int { get }
var SKPaymentTransactionStateFailed: Int { get }
var SKPaymentTransactionStateRestored: Int { get }
var SKPaymentTransactionStateDeferred: Int { get }
typealias SKPaymentTransactionState = Int
@available(OSX 10.7, *)
class SKPaymentTransaction : NSObject {
  var error: NSError? { get }
  var original: SKPaymentTransaction? { get }
  var payment: SKPayment { get }
  var transactionDate: NSDate? { get }
  var transactionIdentifier: String? { get }
  var downloads: [AnyObject]? { get }
  var transactionState: SKPaymentTransactionState { get }
}
