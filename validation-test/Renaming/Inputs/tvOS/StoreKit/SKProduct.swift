
@available(tvOS 3.0, *)
class SKProduct : NSObject {
  @available(tvOS 3.0, *)
  var localizedDescription: String { get }
  @available(tvOS 3.0, *)
  var localizedTitle: String { get }
  @available(tvOS 3.0, *)
  var price: NSDecimalNumber { get }
  @available(tvOS 3.0, *)
  var priceLocale: NSLocale { get }
  @available(tvOS 3.0, *)
  var productIdentifier: String { get }
  @available(tvOS 6.0, *)
  var isDownloadable: Bool { get }
  @available(tvOS 6.0, *)
  var downloadContentLengths: [NSNumber] { get }
  @available(tvOS 6.0, *)
  var downloadContentVersion: String { get }
}
