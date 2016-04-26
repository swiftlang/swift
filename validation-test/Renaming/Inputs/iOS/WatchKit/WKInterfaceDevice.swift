
class WKInterfaceDevice : NSObject {
  @discardableResult
  class func current() -> WKInterfaceDevice
  @available(iOS 8.2, *)
  @discardableResult
  func addCachedImage(_ image: UIImage, name name: String) -> Bool
  @available(iOS 8.2, *)
  @discardableResult
  func addCachedImage(with imageData: NSData, name name: String) -> Bool
  @available(iOS 8.2, *)
  func removeCachedImage(withName name: String)
  @available(iOS 8.2, *)
  func removeAllCachedImages()
  @available(iOS 8.2, *)
  var cachedImages: [String : NSNumber] { get }
  var screenBounds: CGRect { get }
  var screenScale: CGFloat { get }
  var preferredContentSizeCategory: String { get }
  @available(iOS 9.0, *)
  var systemVersion: String { get }
  @available(iOS 9.0, *)
  var name: String { get }
  @available(iOS 9.0, *)
  var model: String { get }
  @available(iOS 9.0, *)
  var localizedModel: String { get }
  @available(iOS 9.0, *)
  var systemName: String { get }
}
