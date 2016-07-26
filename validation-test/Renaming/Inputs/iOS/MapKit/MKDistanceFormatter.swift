
@available(iOS 7.0, *)
class MKDistanceFormatter : NSFormatter {
  @discardableResult
  func string(fromDistance distance: CLLocationDistance) -> String
  @discardableResult
  func distance(from distance: String) -> CLLocationDistance
  @NSCopying var locale: NSLocale!
  var units: MKDistanceFormatterUnits
  var unitStyle: MKDistanceFormatterUnitStyle
}
@available(iOS 7.0, *)
enum MKDistanceFormatterUnits : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case metric
  case imperial
  case imperialWithYards
}
@available(iOS 7.0, *)
enum MKDistanceFormatterUnitStyle : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case `default`
  case abbreviated
  case full
}
