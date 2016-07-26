
@available(iOS 7.0, *)
struct MKDirectionsTransportType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var automobile: MKDirectionsTransportType { get }
  static var walking: MKDirectionsTransportType { get }
  @available(iOS 9.0, *)
  static var transit: MKDirectionsTransportType { get }
  static var any: MKDirectionsTransportType { get }
}
