
@available(OSX 10.7, *)
class SKProduct : NSObject {
  var localizedDescription: String? { get }
  var localizedTitle: String? { get }
  var price: NSDecimalNumber? { get }
  var priceLocale: NSLocale? { get }
  var productIdentifier: String? { get }
  var downloadable: Bool { get }
  var contentVersion: String? { get }
  var contentLengths: [NSNumber]? { get }
}
