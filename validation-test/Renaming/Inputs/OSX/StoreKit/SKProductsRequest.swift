
protocol SKProductsRequestDelegate : SKRequestDelegate {
  @available(OSX 10.7, *)
  func productsRequest(_ request: SKProductsRequest, didReceive response: SKProductsResponse)
}
@available(OSX 10.7, *)
class SKProductsRequest : SKRequest {
  init(productIdentifiers productIdentifiers: Set<NSObject>)
}
@available(OSX 10.7, *)
class SKProductsResponse : NSObject {
  var products: [SKProduct]? { get }
  var invalidProductIdentifiers: [String]? { get }
}
