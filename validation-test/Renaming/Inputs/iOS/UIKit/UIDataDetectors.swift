
struct UIDataDetectorTypes : OptionSet {
  init(rawValue rawValue: UInt)
  let rawValue: UInt
  static var phoneNumber: UIDataDetectorTypes { get }
  static var link: UIDataDetectorTypes { get }
  @available(iOS 4.0, *)
  static var address: UIDataDetectorTypes { get }
  @available(iOS 4.0, *)
  static var calendarEvent: UIDataDetectorTypes { get }
  static var all: UIDataDetectorTypes { get }
}
