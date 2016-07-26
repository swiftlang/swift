
@available(iOS 7.0, *)
let CTRadioAccessTechnologyDidChangeNotification: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyGPRS: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyEdge: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyWCDMA: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyHSDPA: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyHSUPA: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyCDMA1x: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyCDMAEVDORev0: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyCDMAEVDORevA: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyCDMAEVDORevB: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyeHRPD: String
@available(iOS 7.0, *)
let CTRadioAccessTechnologyLTE: String
@available(iOS 4.0, *)
class CTTelephonyNetworkInfo : NSObject {
  @available(iOS 4.0, *)
  var subscriberCellularProvider: CTCarrier? { get }
  @available(iOS 4.0, *)
  var subscriberCellularProviderDidUpdateNotifier: ((CTCarrier) -> Void)?
  @available(iOS 7.0, *)
  var currentRadioAccessTechnology: String? { get }
}
