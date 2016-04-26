
@available(iOS 4.0, *)
class CTCarrier : NSObject {
  @available(iOS 4.0, *)
  var carrierName: String? { get }
  @available(iOS 4.0, *)
  var mobileCountryCode: String? { get }
  @available(iOS 4.0, *)
  var mobileNetworkCode: String? { get }
  @available(iOS 4.0, *)
  var isoCountryCode: String? { get }
  @available(iOS 4.0, *)
  var allowsVOIP: Bool { get }
}
