
@available(OSX 10.7, *)
class SKPayment : NSObject, NSCopying, NSMutableCopying {
  @discardableResult
  class func payment(with product: SKProduct) -> AnyObject
  var productIdentifier: String { get }
  @NSCopying var requestData: NSData? { get }
  var quantity: Int { get }
  var applicationUsername: String? { get }
}
@available(OSX 10.7, *)
class SKMutablePayment : SKPayment {
}
