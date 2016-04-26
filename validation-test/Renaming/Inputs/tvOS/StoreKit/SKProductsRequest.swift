
protocol SKProductsRequestDelegate : SKRequestDelegate {
  @available(tvOS 3.0, *)
  func productsRequest(_ request: SKProductsRequest, didReceive response: SKProductsResponse)
}
@available(tvOS 3.0, *)
class SKProductsRequest : SKRequest {
  @available(tvOS 3.0, *)
  init(productIdentifiers productIdentifiers: Set<String>)
}
@available(tvOS 3.0, *)
class SKProductsResponse : NSObject {
  @available(tvOS 3.0, *)
  var products: [SKProduct] { get }
  @available(tvOS 3.0, *)
  var invalidProductIdentifiers: [String] { get }
}
