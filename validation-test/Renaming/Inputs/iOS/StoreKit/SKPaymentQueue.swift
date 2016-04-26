
@available(iOS 3.0, *)
class SKPaymentQueue : NSObject {
  @available(iOS 3.0, *)
  @discardableResult
  class func defaultQueue() -> Self
  @available(iOS 3.0, *)
  @discardableResult
  class func canMakePayments() -> Bool
  @available(iOS 3.0, *)
  func add(_ payment: SKPayment)
  @available(iOS 3.0, *)
  func restoreCompletedTransactions()
  @available(iOS 7.0, *)
  func restoreCompletedTransactions(withApplicationUsername username: String?)
  @available(iOS 3.0, *)
  func finishTransaction(_ transaction: SKPaymentTransaction)
  @available(iOS 6.0, *)
  func start(_ downloads: [SKDownload])
  @available(iOS 6.0, *)
  func pause(_ downloads: [SKDownload])
  @available(iOS 6.0, *)
  func resumeDownloads(_ downloads: [SKDownload])
  @available(iOS 6.0, *)
  func cancel(_ downloads: [SKDownload])
  @available(iOS 3.0, *)
  func add(_ observer: SKPaymentTransactionObserver)
  @available(iOS 3.0, *)
  func remove(_ observer: SKPaymentTransactionObserver)
  @available(iOS 3.0, *)
  var transactions: [SKPaymentTransaction] { get }
}
protocol SKPaymentTransactionObserver : NSObjectProtocol {
  @available(iOS 3.0, *)
  func paymentQueue(_ queue: SKPaymentQueue, updatedTransactions transactions: [SKPaymentTransaction])
  @available(iOS 3.0, *)
  optional func paymentQueue(_ queue: SKPaymentQueue, removedTransactions transactions: [SKPaymentTransaction])
  @available(iOS 3.0, *)
  optional func paymentQueue(_ queue: SKPaymentQueue, restoreCompletedTransactionsFailedWithError error: NSError)
  @available(iOS 3.0, *)
  optional func paymentQueueRestoreCompletedTransactionsFinished(_ queue: SKPaymentQueue)
  @available(iOS 6.0, *)
  optional func paymentQueue(_ queue: SKPaymentQueue, updatedDownloads downloads: [SKDownload])
}
