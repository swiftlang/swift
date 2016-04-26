
enum CLKComplicationFamily : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case modularSmall
  case modularLarge
  case utilitarianSmall
  case utilitarianLarge
  case circularSmall
}
struct CLKComplicationTimeTravelDirections : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var forward: CLKComplicationTimeTravelDirections { get }
  static var backward: CLKComplicationTimeTravelDirections { get }
}
enum CLKComplicationPrivacyBehavior : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case showOnLockScreen
  case hideOnLockScreen
}
enum CLKComplicationTimelineAnimationBehavior : UInt {
  init?(rawValue rawValue: UInt)
  var rawValue: UInt { get }
  case never
  case grouped
  case always
}
enum CLKComplicationColumnAlignment : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case leading
  case trailing
  @available(watchOS 2.0, *)
  @available(watchOS, deprecated: 2.1, message: "Use CLKComplicationColumnAlignmentLeading instead")
  static var left: CLKComplicationColumnAlignment { get }
  @available(watchOS 2.0, *)
  @available(watchOS, deprecated: 2.1, message: "Use CLKComplicationColumnAlignmentTrailing instead")
  static var right: CLKComplicationColumnAlignment { get }
}
enum CLKComplicationRingStyle : Int {
  init?(rawValue rawValue: Int)
  var rawValue: Int { get }
  case closed
  case open
}
let CLKLaunchedTimelineEntryDateKey: String
