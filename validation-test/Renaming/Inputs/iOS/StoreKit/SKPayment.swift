
@available(iOS 3.0, *)
class SKPayment : NSObject, NSCopying, NSMutableCopying {
  @available(iOS 3.0, *)
  convenience init(product product: SKProduct)
  @available(iOS 3.0, *)
  var productIdentifier: String { get }
  @available(iOS 3.0, *)
  @NSCopying var requestData: NSData? { get }
  @available(iOS 3.0, *)
  var quantity: Int { get }
  @available(iOS 7.0, *)
  var applicationUsername: String? { get }
  @available(iOS 8.3, *)
  var simulatesAskToBuyInSandbox: Bool { get }
}
@available(iOS 3.0, *)
class SKMutablePayment : SKPayment {
}
