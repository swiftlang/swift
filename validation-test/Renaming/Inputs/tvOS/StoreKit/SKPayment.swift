
@available(tvOS 3.0, *)
class SKPayment : NSObject, NSCopying, NSMutableCopying {
  @available(tvOS 3.0, *)
  convenience init(product product: SKProduct)
  @available(tvOS 3.0, *)
  var productIdentifier: String { get }
  @available(tvOS 3.0, *)
  @NSCopying var requestData: NSData? { get }
  @available(tvOS 3.0, *)
  var quantity: Int { get }
  @available(tvOS 7.0, *)
  var applicationUsername: String? { get }
  @available(tvOS 8.3, *)
  var simulatesAskToBuyInSandbox: Bool { get }
}
@available(tvOS 3.0, *)
class SKMutablePayment : SKPayment {
}
