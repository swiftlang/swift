
@available(iOS 3.0, *)
class SKProduct : NSObject {
  @available(iOS 3.0, *)
  var localizedDescription: String { get }
  @available(iOS 3.0, *)
  var localizedTitle: String { get }
  @available(iOS 3.0, *)
  var price: NSDecimalNumber { get }
  @available(iOS 3.0, *)
  var priceLocale: NSLocale { get }
  @available(iOS 3.0, *)
  var productIdentifier: String { get }
  @available(iOS 6.0, *)
  var isDownloadable: Bool { get }
  @available(iOS 6.0, *)
  var downloadContentLengths: [NSNumber] { get }
  @available(iOS 6.0, *)
  var downloadContentVersion: String { get }
}
