
@available(OSX 10.9, *)
struct MKDirectionsTransportType : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var automobile: MKDirectionsTransportType { get }
  static var walking: MKDirectionsTransportType { get }
  @available(OSX 10.11, *)
  static var transit: MKDirectionsTransportType { get }
  static var any: MKDirectionsTransportType { get }
}
