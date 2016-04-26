
@available(tvOS 9.2, *)
struct MKDirectionsTransportType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var automobile: MKDirectionsTransportType { get }
  static var walking: MKDirectionsTransportType { get }
  @available(tvOS 9.0, *)
  static var transit: MKDirectionsTransportType { get }
  static var any: MKDirectionsTransportType { get }
}
