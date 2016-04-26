
@available(OSX 10.7, *)
class SKPaymentQueue : NSObject {
  @discardableResult
  class func defaultQueue() -> SKPaymentQueue
  @discardableResult
  class func canMakePayments() -> Bool
  func add(_ payment: SKPayment)
  func restoreCompletedTransactions()
  func restoreCompletedTransactions(withApplicationUsername username: String?)
  func finishTransaction(_ transaction: SKPaymentTransaction)
  func add(_ observer: SKPaymentTransactionObserver)
  func remove(_ observer: SKPaymentTransactionObserver)
  var transactions: [SKPaymentTransaction]? { get }
  func start(_ downloads: [SKDownload])
  func pause(_ downloads: [SKDownload])
  func resumeDownloads(_ downloads: [SKDownload])
  func cancel(_ downloads: [SKDownload])
}
protocol SKPaymentTransactionObserver : NSObjectProtocol {
  @available(OSX 10.7, *)
  func paymentQueue(_ queue: SKPaymentQueue, updatedTransactions transactions: [SKPaymentTransaction])
  @available(OSX 10.7, *)
  optional func paymentQueue(_ queue: SKPaymentQueue, removedTransactions transactions: [SKPaymentTransaction])
  @available(OSX 10.7, *)
  optional func paymentQueue(_ queue: SKPaymentQueue, restoreCompletedTransactionsFailedWithError error: NSError)
  @available(OSX 10.7, *)
  optional func paymentQueueRestoreCompletedTransactionsFinished(_ queue: SKPaymentQueue)
  @available(OSX 10.8, *)
  optional func paymentQueue(_ queue: SKPaymentQueue, updatedDownloads downloads: [SKDownload])
}
